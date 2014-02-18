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
import Id(isDataConId_maybe)

import HERMIT.Context
import HERMIT.Core (Crumb(..),localFreeIdsExpr)
import HERMIT.External
import HERMIT.Monad
import HERMIT.GHC hiding (mkStringExpr)
import qualified HERMIT.GHC as HGHC
import HERMIT.Kure hiding (apply)
import HERMIT.Optimize

-- Note: All of the Dictionary submodules are now re-exported by HERMIT.Dictionary,
--       so if you prefer you could import all these via that module, rather than seperately.
import HERMIT.Dictionary.AlphaConversion (unshadowR)
import HERMIT.Dictionary.Common -- hiding (findId, findIdT)
--import qualified HERMIT.Dictionary.Common as Common
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

---------------------------

type ReExpr = RewriteH CoreExpr

reifyExpr :: ReExpr
reifyExpr = do
	e@(App (App (Var reifyId) (Type ty)) expr) <- idR
	reifyId' <- findIdT "reifyExpr"	-- need to be more precise
	True <- return $ reifyId == reifyId'
	-- okay, good to translate
	varId     <- findIdT "Language.GHC.Core.Reify.Internals.Var"
	bindeeId  <- findIdT "Language.GHC.Core.Reify.Internals.Bindee_"
	returnId  <- findIdT "Language.GHC.Core.Reify.Internals.returnIO"
	exprTyId  <- findTyIdT "Language.GHC.Core.Reify.Internals.Expr"
	nothingId  <- findIdT "Language.GHC.Core.Reify.Internals.nothing"
	unitId    <- findIdT "()"
	typeId     <- findIdT "Language.GHC.Core.Reify.Internals.Type"
	observeR "ref"
	dynFlags <- constT getDynFlags
	() <- trace ("type : " ++ showPpr dynFlags (HGHC.exprType e)) $ return ()
	(ioTyCon,exprTyCon,eTy) <- case HGHC.exprType e of
		 TyConApp ioTyCon [TyConApp exprTyCon eTy] -> return (ioTyCon,exprTyCon,eTy)
	 	 _ -> error "Internal error; stange type to reify"

	let exprTy e = TyConApp exprTyCon [e]

        let liftName :: Type -> Name -> TranslateH a CoreExpr
            liftName ty nm = mkName (getOccString nm) 0 ty

            -- Assumes Var, not TyVar
            liftId :: Var -> TranslateH a CoreExpr
            liftId var = liftName (HGHC.exprType (Var var)) (idName var)

            -- Type <ty>, phantom
            liftType :: RewriteH CoreExpr
            liftType = do
                    Type ty <- idR
                    return $ apps typeId [ty] []

        let dummy str = do
                nm <- mkName str 0 ty
                return $  apps varId [ty]
	            [ apps bindeeId [ty] [ expr
                                         , apps nothingId [exprTy ty] []
                                         , nm
                                         ]
                    ]                         
                
            liftVar env = do
                e@(Var var) <- idR
                let ty = HGHC.exprType e
                case lookup var env of
                  Nothing -> do
                          nm <- liftId var
                          return $  apps varId [ty]
                            [ apps bindeeId [ty] [ e
                                         , apps nothingId [exprTy ty] []
                                         , nm
                                         ]
                            ]
                  Just e -> return $ e

            liftTyApp env = do
                e@(App f (Type a_ty)) <- idR
                (f',x') <- appT (liftExpr env) liftType (,)
--                let t_ty = HGHC.exprType e
--                appId <- findIdT "Language.GHC.Core.Reify.Internals.TyApp"
                return $ f' -- apps appId [t_ty,a_ty] [ f', x' ]

            liftApp env = do
                -- Assume x is not a type for now
                e@(App f x) <- idR
                (f',x') <- appT (liftExpr env) (liftExpr env) (,)
                let a_ty = HGHC.exprType e
                let b_ty = HGHC.exprType x
                appId <- findIdT "Language.GHC.Core.Reify.Internals.App"
                return $  apps appId [a_ty,b_ty] [ f', x' ]
            
            liftLit env = do
                e@(App (Var intHash) (Lit (MachInt i))) <- idR
                True <- return $ Just intDataCon == isDataConId_maybe intHash
                litId <- findIdT "Language.GHC.Core.Reify.Internals.Lit"
                litIntId <- findIdT "Language.GHC.Core.Reify.Internals.LitInt"
                let ty = HGHC.exprType e
                return $  apps litId [ty] 
	            [ apps litIntId [] [ mkInt i ]]

            liftLam env = do
                -- Assume x is not a type, for now
                e@(Lam var e0) <- idR
                let a_ty = HGHC.exprType (Var var)
                let b_ty = HGHC.exprType e0
                nm <- liftId var 
                newId <- constT (newIdH (getOccString $ idName var) 
                                        (exprTy a_ty))
                (var,e') <- lamT idR (liftExpr ((var,Var newId):env)) (,)
                appId <- findIdT "Language.GHC.Core.Reify.Internals.Lam"
                return $  apps appId [a_ty,b_ty] [ nm, Lam newId e' ]

            liftExpr :: [(Id,CoreExpr)] -> RewriteH CoreExpr
            liftExpr env = liftVar env
                        <+ liftLit env
                        <+ liftTyApp env
                        <+ liftApp env -- after liftLit, which spots some apps
                        <+ liftLam env
                        <+ dummy "no_match"

        appT idR (liftExpr []) $ \ _ expr' -> apps returnId [exprTy ty] [expr']
{-

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
--	traceR $ ("ty" ++ show ty)
--	traceR $ ("expr" ++ show expr)
--	str <- constT (mkStringExpr "mhhha")
--        uq <- 




        liftExpr
--        expr' <- error ""
--	return $ apps returnId [exprTy ty] expr'
-}
                 

mkName :: String -> Integer -> Type -> TranslateH a CoreExpr
mkName str uq ty = do
        nmId <- findTyIdT "Language.GHC.Core.Reify.Internals.Name_"
        return $ apps nmId [ty] [mkString str, mkInt uq]
     
mkString :: String -> CoreExpr        
mkString = foldr (\ a b -> mkConApp consDataCon [Type charTy,mkConApp charDataCon [Lit $ MachChar a],b])
                        (mkConApp nilDataCon [Type charTy])
                
mkInt :: Integer -> CoreExpr
mkInt n = mkConApp intDataCon [Lit $ MachInt n]
