{-# LANGUAGE KindSignatures, GADTs, RankNTypes, ImpredicativeTypes #-}
module Language.GHC.Core.Reify.Internals where

---------------------------------------------------------------------------------

-- | 'Expr' is a HOAS representation of GHC Core.

data Expr :: * -> * where
	Var     :: Bindee a       	 		 -> Expr a
	App     :: Expr (a -> b) -> Expr a 		 -> Expr b
        TyApp   :: Expr (Type a -> b) -> Type a          -> Expr b
        Lit     :: Lit a                                 -> Expr a
        Lam     :: Name a -> (Expr a -> Expr b)          -> Expr (a -> b)
        TyLam   :: (Type a -> Expr b)                    -> Expr (Type a -> b)
        -- The rhs of any binding is 'pre-inlined', inside bindees.
        Let     :: Name a -> Expr a -> (Expr a -> Expr b) -> Expr b
        -- fixpointing
        Fix     :: Name a -> (Expr a -> Expr a)          -> Expr a
        -- evaluation and de-constructing.
        -- The list is of finite and fixed in length.
        Case    :: Name a 
                -> Expr a
                -> [Alt a c]                             -> Expr b

instance Show (Expr a) where
	show (Var b)            = show b
	show (App e1 e2)        = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
	show (TyApp e t)        = "(" ++ show e ++ " @ " ++ show t ++ ")"
        show (Lit i)            = show i
        show (Let nm e0 e1)     = "let{" ++ show nm ++ "=" ++ show e0 ++ "}in " ++ show (e1 $ dummyVar nm)
        show (Fix nm e0)        = "fix{" ++ show nm ++ "=" ++ show (e0 $ dummyVar nm) ++ "}in " ++ show nm
        show (Lam nm f)         = "(\\ " ++ show nm ++ " -> " ++ show (f $ dummyVar nm) ++ ")"

dummyVar :: Name a -> Expr a
dummyVar nm = Var (Bindee_ undefined Nothing nm)

---------------------------------------------------------------------------------

 -- Alts are tried them in order, with no overlapping (except default)

data Alt :: * -> * -> * where
        Alt :: Match a b -> (Expr a -> Expr b -> Expr c) -> Alt a c

data Match :: * -> * -> * where
        Match :: (Expr a -> Expr (Maybe b)) -> Match a b
        DEFAULT                             :: Match a ()

---------------------------------------------------------------------------------

-- | 'Bindee' contains the value, the binding defintion, and the 'Name'.
--   'Bindee' is not a phantom.
data Bindee a = Bindee_ a (Maybe (Expr a)) (Name a)

instance Show (Bindee a) where
	show (Bindee_ _ Nothing nm)  = show nm
	show (Bindee_ _ (Just _) nm) = show nm


---------------------------------------------------------------------------------

data Name a = Name_ String Int 
        
instance Show (Name a) where
   show (Name_ nm uq) = nm ++ "_" ++ show uq

---------------------------------------------------------------------------------

data Lit :: * -> * where
        LitString       :: String             -> Lit String
        LitChar         :: Char               -> Lit Char
        LitInt          :: Int                -> Lit Int

instance Show (Lit a) where
   show (LitString str)         = show str
   show (LitChar ch)            = show ch
   show (LitInt i)              = show i

---------------------------------------------------------------------------------

data Type a = Type

instance Show (Type a) where
        show Type = "<ty>"

---------------------------------------------------------------------------------
        
--  | reify the argument into our HOAS version of Core.
--    'reifyExpr' is an atom in the reify algebra
{-# NOINLINE reifyExpr #-}
reifyExpr :: a -> IO (Expr a)
reifyExpr _ = error "reify called at runtime (should have been removed at compile time)"

evalExpr :: forall a . Expr a -> a
evalExpr (Var nm)       = evalBindee nm
evalExpr (App e1 e2)    = evalExpr e1 (evalExpr e2)
evalExpr (TyApp e1 t)   = evalExpr e1 t
evalExpr (Lit lit)      = evalLit lit
evalExpr (Lam n v)      = \ x -> evalExpr (v (Var (Bindee_ x Nothing n)))
evalExpr (TyLam f)      = \ t -> evalExpr (f t)
        
evalBindee :: Bindee a -> a
evalBindee (Bindee_ a _ _) = a

evalLit :: Lit a -> a
evalLit (LitInt i) = i

inlineBindee :: Bindee a -> Maybe (Expr a)
inlineBindee (Bindee_ _ e _) = e

nameOfBindee :: Bindee a -> Name a
nameOfBindee (Bindee_ _ _ e) = e

-- TODO: find these names properly; we've fixed the HERMIT bug now.
-- Just to make it easier to reify.
returnIO :: a -> IO a
returnIO = return

nothing :: Maybe a
nothing = Nothing
