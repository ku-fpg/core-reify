{-# LANGUAGE KindSignatures, GADTs #-}
module Language.GHC.Core.Reify.Internals where

data Expr :: * -> * where
	Var :: Name a       	 		-> Expr a
	App :: Expr (a -> b) -> Expr a		-> Expr b

instance Show (Expr a) where
	show (Var _) = show "<var>"
	show (App e1 e2) = show e1 ++ " " ++ show e2

-- Not phantom, but real
data Name a = Name a String

--  reify becomes an atom in the reify algebra
{-# NOINLINE reifyExpr #-}
reifyExpr :: a -> IO (Expr a)
reifyExpr _ = error "reify called at runtime (should have been removed at compile time)"

evalExp :: Expr a -> a
evalExp (Var nm) = evalName nm

evalName :: Name a -> a
evalName (Name a _) = a

inlineName :: Name a -> IO (Maybe (Expr a))
inlineName (Name _ _) = error "inline Problem"

