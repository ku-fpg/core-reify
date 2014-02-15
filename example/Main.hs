module Main where
        
import Language.GHC.Core.Reify
import Language.GHC.Core.Reify.Internals
import Language.GHC.Core.Reify.Plugin
import GHC.Base
        
fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

main = do
--        e <- reifyExpr fib
        e <- reifyExpr (99 :: Int)
        print e
