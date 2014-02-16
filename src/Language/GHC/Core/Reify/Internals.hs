{-# LANGUAGE KindSignatures, GADTs #-}
module Language.GHC.Core.Reify.Internals where

data Expr :: * -> * where
	Var :: Bindee a       	 		-> Expr a
	App :: Expr (a -> b) -> Expr a		-> Expr b
        Lit :: Lit a                            -> Expr a
        Lam :: Name a -> (Expr a -> Expr b)     -> Expr (a -> b)
--        Fix :: Name a -> Expr a                 -> Expr a
        
instance Show (Expr a) where
	show (Var b)            = show b
	show (App e1 e2)        = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
        show (Lit i)            = show i
        show (Lam nm f)         = "(\\ " ++ show nm ++ " -> " ++ show (f $ (Var (Bindee_ undefined Nothing nm))) ++ ")"

-- Not phantom, but real
-- contains the value, the binding defintion, the name, and a unique int
data Bindee a = Bindee_ a (Maybe (Expr a)) (Name a)

instance Show (Bindee a) where
	show (Bindee_ _ Nothing nm)  = show nm
	show (Bindee_ _ (Just _) nm) = show nm

data Lit :: * -> * where
        LitString       :: String             -> Lit String
        LitChar         :: Char               -> Lit Char
        LitInt          :: Int                -> Lit Int

instance Show (Lit a) where
   show (LitString str)         = show str
   show (LitChar ch)            = show ch
   show (LitInt i)              = show i

data Name a = Name_ String Int 
        
instance Show (Name a) where
   show (Name_ nm uq) = nm ++ "_" ++ show uq
        
--  reify becomes an atom in the reify algebra
{-# NOINLINE reifyExpr #-}
reifyExpr :: a -> IO (Expr a)
reifyExpr _ = error "reify called at runtime (should have been removed at compile time)"

evalExp :: Expr a -> a
evalExp (Var nm) = evalBindee nm

evalBindee :: Bindee a -> a
evalBindee (Bindee_ a _ _) = a

inlineBindee :: Bindee a -> Maybe (Expr a)
inlineBindee (Bindee_ _ e _) = e

nameOfBindee :: Bindee a -> Name a
nameOfBindee (Bindee_ _ _ e) = e

-- Just to make it easier to reify.
returnIO :: a -> IO a
returnIO = return

nothing :: Maybe a
nothing = Nothing
