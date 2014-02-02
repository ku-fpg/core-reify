{-# LANGUAGE GADTs #-}

-- | 'Exp' are finite syntax trees, based on what core can do.

data Exp :: * -> * where
     App  :: Exp (a -> b) -> Exp a		-> Exp b
     Var  :: Name a   	     	 		-> Exp a
     Case :: Expr a -> [(Pat a,Expr b)]		-> Expr b
     Lam  :: Name a -> Expr b 	    		-> Expr (a -> b)
     Type :: Type a    	    			-> Expr (Type a)
     Let    :: Name a -> Expr a -> Expr b	-> Expr b
     LetRec :: [Def]  -> Expr b			-> Expr b

data Type a = ...

data Def where
     Def :: Name a -> Expr a -> Def

-- How do we represent types? or contraints? 

data Pat :: * -> * where
data Type :: * -> * where
data Name :: * -> * where

typeOf :: Exp a -> Type a
typeOf = undefined

instance Eq where
instance Ord where
instance Show where

reify :: a -> IO (Exp a)
reify = undefined

eval :: Exp a -> a
eval (App e1 e2) = eval e1 (eval e2)
eval (Lam n e) = 


fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

myMap :: (a -> b) -> [a] -> [b]
myMap = undefined

main = do
     f <- reify fib
     print f     


fibE :: Exp (Int -> Int)
fibE = 
