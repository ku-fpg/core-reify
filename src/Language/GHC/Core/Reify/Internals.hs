{-# LANGUAGE KindSignatures, GADTs #-}
module Language.GHC.Core.Reify.Internals where

data Expr :: * -> * where
	Var :: Bindee a       	 		-> Expr a
	App :: Expr (a -> b) -> Expr a		-> Expr b

instance Show (Expr a) where
	show (Var _) = show "<var>"
	show (App e1 e2) = show e1 ++ " " ++ show e2

-- Not phantom, but real
data Bindee a = Bindee_ a --  String

--  reify becomes an atom in the reify algebra
{-# NOINLINE reifyExpr #-}
reifyExpr :: a -> IO (Expr a)
reifyExpr _ = error "reify called at runtime (should have been removed at compile time)"

evalExp :: Expr a -> a
evalExp (Var nm) = evalBindee nm

evalBindee :: Bindee a -> a
evalBindee (Bindee_ a) = a

--inlineName :: Name a -> IO (Maybe (Expr a))
--inlineName (Name _ _) = error "inline Problem"

-- Just to make it easier to reify.
returnIO :: a -> IO a
returnIO = return
