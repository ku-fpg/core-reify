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
import HERMIT.Core (Crumb(..),localFreeIdsExpr,CoreDef(..))
import HERMIT.External
import HERMIT.Monad
import HERMIT.GHC hiding (mkStringExpr)
import qualified HERMIT.GHC as HGHC
import HERMIT.Kure hiding (apply)
import HERMIT.Plugin

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
plugin = hermitPlugin (phase 0 . interactive externals)

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
findTyConT :: (BoundVars c, HasGlobalRdrEnv c, HasDynFlags m, MonadThings m, MonadCatch m) => String -> Translate c m a TyCon
findTyConT nm = prefixFailMsg ("Cannot resolve name " ++ nm ++ ", ") $
             contextonlyT (findTyConMG nm)

findTyConMG :: (BoundVars c, HasGlobalRdrEnv c, HasDynFlags m, MonadThings m) => String -> c -> m TyCon
findTyConMG nm c =
    case filter isTyConName $ findNamesFromString (hermitGlobalRdrEnv c) nm of
      o | traceShow ("findTyConMG",nm,length o) False -> undefined
      [n] -> lookupTyCon n
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
	exprTyCon  <- findTyConT "Language.GHC.Core.Reify.Internals.Expr"
	nothingId <- findIdT "Language.GHC.Core.Reify.Internals.nothing"
	unitId    <- findIdT    "()"
	typeId    <- findIdT    "Language.GHC.Core.Reify.Internals.Type"
	typeTyCon  <- findTyConT  "Language.GHC.Core.Reify.Internals.Type"
	observeR "ref"
	dynFlags <- constT getDynFlags
	() <- trace ("type : " ++ showPpr dynFlags (HGHC.exprType e)) $ return ()
	(ioTyCon,exprTyCon,eTy) <- case HGHC.exprType e of
		 TyConApp ioTyCon [TyConApp exprTyCon eTy] -> return (ioTyCon,exprTyCon,eTy)
	 	 _ -> error "Internal error; stange type to reify"
	() <- trace ("type : " ++ showPpr dynFlags typeTyCon) $ return ()
	let exprTy e = TyConApp exprTyCon [e]
	let typeTy e = mkAppTy (mkTyConTy typeTyCon) e

                    -- (\/ a . a -> a) ==> ([a],Type a -> a -> a)
        let reifiedType ty = (tys,foldr fakeTypes inside tys)
                where (tys,inside) = splitForAllTys ty
                      fakeTypes :: TyVar -> Type -> Type
                      fakeTypes v t = mkFunTy (typeTy (mkTyVarTy v)) t

                    -- (\/ a . a -> a) ==> (Type a -> a -> a)
                    -- The assumption is that someone else will provide
                    -- the \/ a. ...
        let normalizeTy ty = ty'
                where (foralls,ty') = reifiedType ty


        let liftName :: Type -> Name -> TranslateH a CoreExpr
            liftName ty nm = mkName (getOccString nm) 0 ty

            -- Assumes Var, not TyVar
            liftId :: Var -> TranslateH a CoreExpr
            liftId var = liftName (normalizeTy (HGHC.exprType (Var var))) (idName var)

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
                case lookup var env of
                  Nothing -> do
                          let (foralls,ty) = reifiedType $ HGHC.exprType e
                          nm <- liftId var
                          -- e' = \ Type -> id ty
                          let e' = mkLams [ mkWildValBinder (typeTy (mkTyVarTy v)) | v <- foralls ]
                                 $ mkTyApps e
                                 $ [ mkTyVarTy v | v <- foralls ]
                          return $ mkLams foralls
                                 $ apps varId [ty]
                                 $ [ apps bindeeId [ty] 
                                    [ e'
                                    , apps nothingId [exprTy ty] []
                                    , nm
                                    ]
                                   ]
                  Just e -> return $ e

            liftTyApp env = do
                e@(App f (Type a_ty)) <- idR
                (f',x') <- appT (liftExpr env) liftType (,)
                let (forAlls,t_ty) = reifiedType (HGHC.exprType f)
                Just (_,lhs_t_ty) <- return $ splitAppTy_maybe t_ty
                (hd:tl) <- return $ forAlls
                appId <- findIdT "Language.GHC.Core.Reify.Internals.TyApp"
                return $ mkLams tl 
                       $ mkLets [mkTyBind hd a_ty]
                       $ apps appId [lhs_t_ty,mkTyVarTy hd] 
                       $ [ mkTyApps f' [mkTyVarTy v | v <- forAlls], x' ]

            liftApp env = do
                -- Assume x is not a type for now
                e@(App f x) <- idR
                (f',x') <- appT (liftExpr env) (liftExpr env) (,)
                let (_,a_ty) = reifiedType (HGHC.exprType e)
                let (_,b_ty) = reifiedType (HGHC.exprType x)   -- no rank-2 polymorphism here
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


            liftNonRecLet env = do
                -- Assume var is not a type for now
                e@(Let (NonRec var e0) e1) <- idR

                let (_,a_ty) = reifiedType (HGHC.exprType e0)
                let (_,b_ty) = reifiedType (HGHC.exprType e1)

                nm <- liftId var 
                newId <- constT (newIdH (getOccString $ idName var)
                                        (exprTy a_ty))

                (_,e0',e1') <- letNonRecT
                                (idR)
                                (liftExpr env)                   -- same env
                                (liftExpr ((var,Var newId):env)) -- modified env
                                (,,)

                letId <- findIdT "Language.GHC.Core.Reify.Internals.Let"
                return $  apps letId [b_ty,a_ty] [ nm, e0', Lam newId e1' ]

            liftRecLet env = do
                -- Assume var is not a type; and we have simple recursion
                e@(Let (Rec [(var,e0)]) e1) <- idR     

                let (_,a_ty) = reifiedType (HGHC.exprType e0)
                let (_,b_ty) = reifiedType (HGHC.exprType e1)

                nm <- liftId var 
                newId <- constT (newIdH (getOccString $ idName var)
                                        (exprTy a_ty))

                ([e0'],e1') <- letRecT
                                (\ _ -> liftDef ((var,Var newId):env)) -- modified env 
                                (liftExpr ((var,Var newId):env)) -- modified env
                                (,)

                letId <- findIdT "Language.GHC.Core.Reify.Internals.Let"
                return $  apps letId [b_ty,a_ty] [ nm, e0', Lam newId e1' ]

                -- The first element of the env is the *rec* name we are defining
            liftDef :: [(Id,CoreExpr)] -> TranslateH CoreDef CoreExpr
            liftDef env@((var,Var newId):_) = do
                Def _ e0 <- idR    
                    
                let (_,a_ty) = reifiedType (HGHC.exprType e0)
                nm <- liftId var 
                e0' <- defT idR
                            (liftExpr env) -- modified env
                            (\ _ e -> e)

                fixId <- findIdT "Language.GHC.Core.Reify.Internals.Fix"
                return $  apps fixId [a_ty] [ nm, Lam newId e0' ]



            liftExpr :: [(Id,CoreExpr)] -> RewriteH CoreExpr
            liftExpr env = liftVar env
                        <+ liftLit env
                        <+ liftTyApp env
                        <+ liftApp env -- after liftLit, which spots some apps
                        <+ liftLam env
                        <+ liftNonRecLet env -- (normalizeNonRec >>> liftExpr env)
                        <+ liftRecLet env -- (normalizeNonRec >>> liftExpr env)
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
        nmId <- findIdT "Language.GHC.Core.Reify.Internals.Name_"
        return $ apps nmId [ty] [mkString str, mkInt uq]
     
mkString :: String -> CoreExpr        
mkString = foldr (\ a b -> mkConApp consDataCon [Type charTy,mkConApp charDataCon [Lit $ MachChar a],b])
                        (mkConApp nilDataCon [Type charTy])
                
mkInt :: Integer -> CoreExpr
mkInt n = mkConApp intDataCon [Lit $ MachInt n]
