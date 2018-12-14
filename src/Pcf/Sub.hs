{-# LANGUAGE UndecidableInstances #-}

module Pcf.Sub where

import           Control.Monad      (ap)
import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..))
import           Data.Vector        (Vector)
import qualified Data.Vector        as V

-- UnderScope

data UnderScope f e a =
      ScopeB Int
    | ScopeF a
    | ScopeA Int e
    | ScopeE (f e)
    deriving (Eq, Show)

instance Functor f => Bifunctor (UnderScope f) where
    bimap _ _ (ScopeB b)   = ScopeB b
    bimap _ g (ScopeF a)   = ScopeF (g a)
    bimap f _ (ScopeA i e) = ScopeA i (f e)
    bimap f _ (ScopeE fe)  = ScopeE (f <$> fe)

instance Foldable f => Bifoldable (UnderScope f) where
    bifoldr _ _ z (ScopeB _)   = z
    bifoldr _ g z (ScopeF a)   = g a z
    bifoldr f _ z (ScopeA _ e) = f e z
    bifoldr f _ z (ScopeE fe)  = foldr f z fe

instance Traversable f => Bitraversable (UnderScope f) where
    bitraverse _ _ (ScopeB b)   = pure (ScopeB b)
    bitraverse _ g (ScopeF a)   = ScopeF <$> g a
    bitraverse f _ (ScopeA i e) = ScopeA i <$> f e
    bitraverse f _ (ScopeE fe)  = ScopeE <$> traverse f fe

-- Scope

newtype Scope f a = Scope { unScope :: UnderScope f (Scope f a) a }

wrapScope :: f (Scope f a) -> Scope f a
wrapScope = Scope . ScopeE

liftScope :: Functor f => f a -> Scope f a
liftScope = wrapScope . (pure <$>)

instance (Eq (f (Scope f a)), Eq a) => Eq (Scope f a) where
    Scope u == Scope v = u == v

instance (Show (f (Scope f a)), Show a) => Show (Scope f a) where
    showsPrec d (Scope u) = showsPrec d u

instance Functor f => Functor (Scope f) where
    fmap f (Scope us) = Scope (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope f) where
    foldr f z (Scope us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope f) where
    traverse f (Scope us) = Scope <$> bitraverse (traverse f) f us

instance Functor f => Applicative (Scope f) where
    pure = Scope . ScopeF
    (<*>) = ap

-- TODO this should accumulate # of binders and avoid capture
instance Functor f => Monad (Scope f) where
    return = pure
    Scope us >>= f =
        case us of
            ScopeB b   -> Scope (ScopeB b)
            ScopeF a   -> f a
            ScopeA i e -> e >>= f
            ScopeE fe  -> Scope (ScopeE ((>>= f) <$> fe))

-- Subable

class Subable f where
    abstract :: Eq a => Vector a -> f a -> f a

    instantiate :: Vector (f a) -> f a -> Maybe (f a)

abstract1 :: (Subable f, Eq a) => a -> f a -> f a
abstract1 k = abstract (V.singleton k)

instantiate1 :: Subable f => f a -> f a -> Maybe (f a)
instantiate1 v = instantiate (V.singleton v)

-- Scope Subable

abstractScope :: (Functor f, Eq a) => Int -> Vector a -> Scope f a -> Scope f a
abstractScope num ks s =
    case unScope s of
        ScopeB b -> Scope (ScopeB (num + b))
        ScopeF a ->
            case V.elemIndex a ks of
                Nothing -> s
                Just b  -> Scope (ScopeB (num + b))
        ScopeA nb s' -> let num' = num + nb in Scope (ScopeA num' (abstractScope num' ks s'))
        ScopeE fs -> Scope (ScopeE (abstractScope num ks <$> fs))

instantiateScope :: Traversable f => Int -> Vector (Scope f a) -> Scope f a -> Maybe (Scope f a)
instantiateScope num vs s =
    case unScope s of
        ScopeB b -> undefined
        -- ScopeF _ -> Just s
        -- ScopeA nb s' -> instantiateScope (num + nb) vs s'
        -- ScopeE fs -> Scope . ScopeE  <$> (traverse (instantiateScope num vs) fs)

instance Traversable f => Subable (Scope f) where
    abstract ks = let num = V.length ks in Scope . ScopeA num . abstractScope num ks

    instantiate = instantiateScope 0

-- Name

data Name n a = Name n a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()

-- Exp

data Bind = Rec | NRec deriving (Eq, Show)

data ExpF n a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    | Abst Bind (NameOnly n) a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Exp n a = Scope (ExpF n) a

-- Smart constructors for Exp

var :: a -> Exp n a
var = pure

abst :: Eq a => Bind -> n -> a -> Exp n a -> Exp n a
abst b n a = wrapScope . Abst b (Name n ()) . abstract1 a

lam :: Eq a => n -> a -> Exp n a -> Exp n a
lam = abst NRec

lam' :: Eq n => n -> Exp n n -> Exp n n
lam' n = lam n n

fix :: Eq a => n -> a -> Exp n a -> Exp n a
fix = abst Rec

fix' :: Eq n => n -> Exp n n -> Exp n n
fix' n = fix n n

app :: Exp n a -> Exp n a -> Exp n a
app l r = wrapScope (App l r)

ifz :: Exp n a -> Exp n a -> Exp n a -> Exp n a
ifz g t e = wrapScope (Ifz g t e)

zero :: Exp n a
zero = wrapScope Zero

suc :: Exp n a -> Exp n a
suc = wrapScope  . Suc
