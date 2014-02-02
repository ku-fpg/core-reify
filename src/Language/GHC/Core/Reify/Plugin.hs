{-# LANGUAGE TemplateHaskell #-}
module Language.GHC.Core.Reify.Plugin (plugin) where

import Data.Functor ((<$>))
import Control.Applicative (Applicative(..))
-- import Control.Monad ((<=<),liftM2)
import Control.Arrow (arr,(>>>),(&&&))
import qualified Data.Map as M
import Text.Printf (printf)

import qualified Language.Haskell.TH as TH (Name,mkName)
import qualified Language.Haskell.TH.Syntax as TH (showName)

-- GHC API
import PrelNames (unitTyConKey,boolTyConKey,intTyConKey)

import HERMIT.Context
  (ReadBindings,AddBindings,HermitBindingSite(..),hermitBindings)
import HERMIT.Core (Crumb(..),localFreeIdsExpr)
import HERMIT.External
import HERMIT.GHC hiding (mkStringExpr)
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
 
-------------------------

type ReExpr = RewriteH CoreExpr

reifyExpr :: ReExpr
reifyExpr = do
	reifyId' <- findIdT "reifyExpr"	-- need to be more precise
	e@(App (App (Var reifyId) (Type ty)) expr) <- idR
	True <- return $ reifyId == reifyId'
	-- okay, good to translate
	id <- findIdT "CoreSyn.Var"
	observeR "ref"
--	traceR $ ("ty" ++ show ty)
--	traceR $ ("expr" ++ show expr)
	return $ e


