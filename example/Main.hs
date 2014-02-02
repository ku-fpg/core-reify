module Main where
        
import Language.GHC.Core.Reify
        
fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

main = do
        e <- reifyExpr fib
        print e
