module Main where
        
import Language.GHC.Core.Reify
import Language.GHC.Core.Reify.Internals
import Language.GHC.Core.Reify.Plugin
import GHC.Base
        
fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)



appE = Lam (Name_ "v" 0) (\ x -> App tyAppE  x)

tyAppE = TyApp idE (Type :: Type Int)

idE = (Var (Bindee_ (\ _ -> id) Nothing (Name_ "id" 0))) :: Expr (Type a -> a -> a)

main = do
--        e <- reifyExpr fib
--        e <- reifyExpr (99 :: Int)
--        e <- reifyExpr (fib 99)
--        e <- reifyExpr (\ x -> x :: Int)
--        e <- reifyExpr ((\x -> x)(\ x -> x :: Int) 99)
--        e <- reifyExpr (\ x y -> x + y::Int)
--        e <- reifyExpr (id (99::Int))
--        e <- reifyExpr (id :: Int -> Int)
--        e <- reifyExpr (const (99::Int) (True::Bool))
--        e <- reifyExpr (\ x y -> (x && y,x || y))
--        e <- reifyExpr (let f = \ x y -> (x && y,x || y) in (f,f))
--        e <- reifyExpr ([1,2,3]::[Int])
        e <- reifyExpr (let xs = 1 : xs :: [Int] in xs)
        print e
        print "done"

{-
module main:Main where
  idE ∷ ∀ a . Expr (Type a → a → a)
  tyAppE ∷ Expr (Int → Int)
  appE ∷ Expr (Int → Int)
  fib ∷ Int → Int
  main ∷ IO ()
  main ∷ IO ()
hermit<4> binding-of 'tyAppE
tyAppE ∷ Expr (Int → Int)
tyAppE = TyApp (Int → Int) Int (idE Int) (Type Int)
hermit<5> top
module main:Main where
  idE ∷ ∀ a . Expr (Type a → a → a)
  tyAppE ∷ Expr (Int → Int)
  appE ∷ Expr (Int → Int)
  fib ∷ Int → Int
  main ∷ IO ()
  main ∷ IO ()
hermit<6> binding-of 'idE
idE ∷ ∀ a . Expr (Type a → a → a)
idE = λ a →
  Var
    (Type a → a → a)
    (Bindee_
       (Type a → a → a)
       (λ ds →
          case ds of wild (a → a)
            Type → id a)
       (Nothing (Expr (Type a → a → a)))
       (Name_ (Type a → a → a) (unpackCString# "id") (I# 0)))
        
-}
{-
        
 let f = /\ t \ a -> a
 in f Int x
 
 [f]     => Expr (\/ a . a -> a)
 [f t]   => Expr (t -> t)
 [f t x] => Expr t
        
 -}