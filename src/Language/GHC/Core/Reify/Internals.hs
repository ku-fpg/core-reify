{-# LANGUAGE KindSignatures, GADTs, RankNTypes, ImpredicativeTypes #-}
module Language.GHC.Core.Reify.Internals where

data ForAll (t :: * -> *) = ForAll (forall a . t a)

instance Show (ForAll t) where show _ = "ForAll"

data Expr :: * -> * where
	Var     :: Bindee a       	 		-> Expr a
	App     :: Expr (a -> b) -> Expr a		-> Expr b
        TyApp   :: Expr (Type a -> b) -> Type a         -> Expr b
        Lit     :: Lit a                                -> Expr a
        Lam     :: Name a -> (Expr a -> Expr b)         -> Expr (a -> b)
--        TyLam   :: Expr (t a)                           -> Expr (ForAll t)
        TyLam   :: (Type a -> Expr b)                   -> Expr (Type a -> b)
        Fix :: Name a -> (Expr a -> Expr a)             -> Expr a
        Case :: Expr a
             -> Name a                  -- default of case
             -> [Alt a c]               -- all the alts, try them in order,
                                        -- with no overlapping (except default)
             -> Expr b

data Alt :: * -> * -> * where
        Alt :: Match a b -> (Expr a -> Expr b -> Expr c) -> Alt a c

data Match :: * -> * -> * where
        Match :: (Expr a -> Expr (Maybe b)) -> Match a b
        DEFAULT                             :: Match a ()

-- This is the smart constructor for TyLam, that takes off ForAll constructor
--mkTyLam :: Expr (t a :: *) -> Expr (forall a . (t :: * -> *) a)
--mkTyLam e = unboxForAll (TyLam e)

--unboxForAll :: Expr (ForAll t) -> Expr (forall a . t a)
--unboxForAll = undefined

promote :: (forall a . t a) -> (forall a . Type a -> t a)
promote x Type = x

lam :: (Expr a -> Expr b) -> Expr (a -> b)
lam = undefined

tylam :: (Type a -> Expr b) -> Expr (Type a -> b)
tylam = undefined
        
instance Show (Expr a) where
	show (Var b)            = show b
	show (App e1 e2)        = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
	show (TyApp e t)        = "(" ++ show e ++ " @ " ++ show t ++ ")"
        show (Lit i)            = show i
        show (Lam nm f)         = "(\\ " ++ show nm ++ " -> " ++ show (f $ (Var (Bindee_ undefined Nothing nm))) ++ ")"
--        show (Case e n )

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

data Type a = Type

instance Show (Type a) where
        show Type = "<ty>"


        
--  reify becomes an atom in the reify algebra
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

appTy :: (forall a . t a) -> Type a -> t a
appTy f Type = f

class Spec m where
  spec :: m (forall a . t a) -> Type a -> m (t a)
  spec' :: m (ForAll t) -> Type a -> m (t a)
  gen :: m (t a) -> m (forall a . t a)
--  unbox :: m (ForAll t) -> m (forall a . t a)

instance Spec Expr where
  spec (Var nm) t = Var (spec nm t)
--  spec
--  spec' (App e1 e2) t = App (spec' e1 Type) (spec' e2 Type)
--  spec (TyApp e1 t') t = TyApp e1 Type
  gen (Var nm) = Var (gen nm)



instance Spec Bindee where
  spec (Bindee_ a opt_e n) t = Bindee_ a (fmap (flip spec t) opt_e) (spec n t)

instance Spec Name where
  spec (Name_ a b) t = Name_ a b
  
--  a (Maybe (Expr a)) (Name a)
specBindee :: Bindee (forall a . t a) -> Type a -> Bindee (t a)
specBindee = undefined
        
evalBindee :: Bindee a -> a
evalBindee (Bindee_ a _ _) = a

evalLit :: Lit a -> a
evalLit (LitInt i) = i


inlineBindee :: Bindee a -> Maybe (Expr a)
inlineBindee (Bindee_ _ e _) = e

nameOfBindee :: Bindee a -> Name a
nameOfBindee (Bindee_ _ _ e) = e

-- Just to make it easier to reify.
returnIO :: a -> IO a
returnIO = return

nothing :: Maybe a
nothing = Nothing

-----

data I a = I a          -- forall a . a
data K a b = K a deriving Show
data S f g x = S (f x (g x))
data F a = F (a -> Int) -- forall a . a -> Int
data T a = T (a -> (forall b . b -> (a,b))) -- forall a . a -> b -> (a,b)
type X a = a -> Int
type IT a = a