module Main where
        
import Language.GHC.Core.Reify
import Language.GHC.Core.Reify.Internals
import Language.GHC.Core.Reify.Plugin
import GHC.Base
        
fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

main = do
--        e <- reifyExpr fib
--        e <- reifyExpr (99 :: Int)
--        e <- reifyExpr (fib 99)
--        e <- reifyExpr (\ x -> x :: Int)
--        e <- reifyExpr ((\x -> x)(\ x -> x :: Int) 99)
--        e <- reifyExpr (\ x y -> x + y::Int)
        e <- reifyExpr (id (99::Int))
        print e

{-
        
 let f = /\ t \ a -> a
 in f Int x
 
 [f]     => Expr (\/ a . a -> a)
 [f t]   => Expr (t -> t)
 [f t x] => Expr t
        
 -}