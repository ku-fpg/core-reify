{-# LANGUAGE TemplateHaskell #-}
module Language.GHC.Core.Reify.Plugin (plugin) where

import Data.Functor ((<$>))
import Control.Applicative (Applicative(..))
-- import Control.Monad ((<=<),liftM2)
import Control.Arrow (arr,(>>>),(&&&))
import qualified Data.Map as M
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Maybe (Maybe(..))

import qualified Language.Haskell.TH as TH (Name,mkName)
import qualified Language.Haskell.TH.Syntax as TH (showName)

-- GHC API
import PrelNames (unitTyConKey,boolTyConKey,intTyConKey)

import HERMIT.Context
import HERMIT.Core (Crumb(..),localFreeIdsExpr)
import HERMIT.External
import HERMIT.GHC hiding (mkStringExpr)
import qualified HERMIT.GHC as HGHC
import HERMIT.Kure hiding (apply)
import HERMIT.Optimize

-- Note: All of the Dictionary submodules are now re-exported by HERMIT.Dictionary,
--       so if you prefer you could import all these via that module, rather than seperately.
import HERMIT.Dictionary.AlphaConversion (unshadowR)
import HERMIT.Dictionary.Common
import HERMIT.Dictionary.Composite (simplifyR)
import HERMIT.Dictionary.Debug (observeR,traceR)
--import HERMIT.Dictionary.GHC (rule,rules)
import HERMIT.Dictionary.Inline (inlineNameR, inlineNamesR)
import HERMIT.Dictionary.Local (letIntroR,letFloatArgR,letFloatTopR)
import HERMIT.Dictionary.Navigation (rhsOfT,parentOfT,bindingGroupOfT)
import HERMIT.Dictionary.Composite (simplifyR)
import HERMIT.Dictionary.Unfold (cleanupUnfoldR) -- unfoldNameR,

import Debug.Trace

import CoreSyn
--import LambdaCCC.Misc (Unop,Binop)
--import qualified LambdaCCC.Ty     as T
--import qualified LambdaCCC.Lambda as E
--import LambdaCCC.MkStringExpr (mkStringExpr)


import Control.Monad (liftM)
import Data.Char (ord)

import GhcPlugins hiding (mkStringExpr)
import PrelNames (unpackCStringName,unpackCStringUtf8Name)

import qualified Language.GHC.Core.Reify.Internals as I

plugin :: Plugin
plugin = optimize (phase 0 . interactive externals)

externals :: [External]
externals =
    [ external "reify-core"
        (promoteExprR reifyExpr :: RewriteH Core)
        ["Reify a Core expression into a GADT construction"]
    ]
    
-------------------------

-- | Create a 'CoreExpr' that evaluates to the given string
mkStringExpr :: MonadThings m => String -> m CoreExpr
mkStringExpr str = liftM mk (lookupId unpackName)
 where
   mk unpackId = App (Var unpackId) (Lit (mkMachString str))
   unpackName | all safeChar str = unpackCStringName
              | otherwise        = unpackCStringUtf8Name
   safeChar c = ord c >= 1 && ord c <= 0x7F
 
{--------------------------------------------------------------------
    Core utilities
--------------------------------------------------------------------}

apps :: Id -> [Type] -> [CoreExpr] -> CoreExpr
apps f ts es
  | tyArity f /= length ts =
      error $ printf "apps: Id %s wants %d type arguments but got %d."
                     (var2String f) arity ntys
  | otherwise = mkCoreApps (varToCoreExpr f) (map Type ts ++ es)
 where
   arity = tyArity f
   ntys  = length ts

tyArity :: Id -> Int
tyArity = length . fst . splitForAllTys . varType


-- | Lookup the name in the context first, then, failing that, in GHC's global reader environment.
findTyIdT :: (BoundVars c, HasGlobalRdrEnv c, HasDynFlags m, MonadThings m, MonadCatch m) => String -> Translate c m a Id
findTyIdT nm = prefixFailMsg ("Cannot resolve name " ++ nm ++ ", ") $
             contextonlyT (findTyId nm)

findTyId :: (BoundVars c, HasGlobalRdrEnv c, HasDynFlags m, MonadThings m) => String -> c -> m Id
findTyId nm c = case varSetElems (findBoundVars nm c) of
                []         -> findTyIdMG nm c
                [v]        -> return v
                _ : _ : _  -> fail "multiple matching variables in scope."

findTyIdMG :: (BoundVars c, HasGlobalRdrEnv c, HasDynFlags m, MonadThings m) => String -> c -> m Id
findTyIdMG nm c =
    case findNamesFromString (hermitGlobalRdrEnv c) nm of
      o | traceShow ("findTyIdMG",length o) False -> undefined
      [n] -> lookupId n
      ns  -> do dynFlags <- getDynFlags
                fail $ "multiple matches found:\n" ++ intercalate ", " (map (showPpr dynFlags) ns)

-------------------------

type ReExpr = RewriteH CoreExpr

reifyExpr :: ReExpr
reifyExpr = do
	reifyId' <- findIdT "reifyExpr"	-- need to be more precise
	e@(App (App (Var reifyId) (Type ty)) expr) <- idR
	True <- return $ reifyId == reifyId'
	-- okay, good to translate
	varId     <- findIdT "Language.GHC.Core.Reify.Internals.Var"
	bindeeId  <- findIdT "Language.GHC.Core.Reify.Internals.Bindee_"
	returnId  <- findIdT "Language.GHC.Core.Reify.Internals.returnIO"
	exprTyId  <- findTyIdT "Language.GHC.Core.Reify.Internals.Expr"
	nothingId  <- findIdT "Language.GHC.Core.Reify.Internals.nothing"
	unitId    <- findIdT "()"
	observeR "ref"
	dynFlags <- constT getDynFlags
	() <- trace ("type : " ++ showPpr dynFlags (HGHC.exprType e)) $ return ()

{-
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [KindOrType]
  | FunTy Type Type
  | ForAllTy Var Type
  | LitTy TyLit

	
	-}	
	(ioTyCon,exprTyCon,eTy) <- case HGHC.exprType e of
		 TyConApp ioTyCon [TyConApp exprTyCon eTy] -> return (ioTyCon,exprTyCon,eTy)
	 	 _ -> error "Internal error; stange type to reify"

	let exprTy e = TyConApp exprTyCon [e]
--	traceR $ ("ty" ++ show ty)
--	traceR $ ("expr" ++ show expr)
--	str <- constT (mkStringExpr "mhhha")
	return $ apps returnId [exprTy ty]
	         [ apps varId [ty] 
	            [ apps bindeeId [ty] [expr,apps nothingId [exprTy ty] []]]
		 ]


