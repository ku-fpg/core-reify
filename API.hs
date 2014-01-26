{-# LANGUAGE GADTs #-}

-- | 'Exp' are finite syntax trees, based on what core can do

data Exp :: * -> * where
     App :: Exp (a -> b) -> Exp a -> Exp b
     Var :: String -> Exp a
     Case :: Expr a -> [(Pat a,Expr b)] -> Expr b

data Pat :: * -> * where

instance Eq where
instance Ord where
instance Show where

reify :: a -> IO (Exp a)
reify = undefined

eval :: Exp a -> a
eval = undefined

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

main = do
     f <- reify fib
     print f     
