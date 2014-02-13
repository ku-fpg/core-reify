{-# LANGUAGE KindSignatures, GADTs #-}
module Language.GHC.Core.Reify.Internals where

data Expr :: * -> * where
	Var :: Bindee a       	 		-> Expr a
	App :: Expr (a -> b) -> Expr a		-> Expr b

instance Show (Expr a) where
	show (Var b) = show b
	show (App e1 e2) = show e1 ++ " " ++ show e2

-- Not phantom, but real
data Bindee a = Bindee_ a (Maybe (Expr a))--  String

instance Show (Bindee a) where
	show (Bindee_ _ Nothing)  = "-"
	show (Bindee_ _ (Just _)) = "*"

        
--  reify becomes an atom in the reify algebra
{-# NOINLINE reifyExpr #-}
reifyExpr :: a -> IO (Expr a)
reifyExpr _ = error "reify called at runtime (should have been removed at compile time)"

evalExp :: Expr a -> a
evalExp (Var nm) = evalBindee nm

evalBindee :: Bindee a -> a
evalBindee (Bindee_ a _) = a

inlineName :: Bindee a -> Maybe (Expr a)
inlineName (Bindee_ _ e) = e

-- Just to make it easier to reify.
returnIO :: a -> IO a
returnIO = return

nothing :: Maybe a
nothing = Nothing
