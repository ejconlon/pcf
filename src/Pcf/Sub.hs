{-# LANGUAGE UndecidableInstances #-}

module Pcf.Sub where

import           Control.Monad       (ap)
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Bitraversable  (Bitraversable (..))
import           Data.Foldable       (toList)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

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

varScope :: a -> Scope f a
varScope = Scope . ScopeF

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
    pure = varScope
    (<*>) = ap

instance MonadTrans Scope where
    lift = liftScope

-- TODO These are wrong, need to review shifting and fix

scopeShift :: Functor f => Int -> Scope f a -> Scope f a
scopeShift n s@(Scope us) =
    case us of
        ScopeB b -> Scope (ScopeB (n + b))
        ScopeF _ -> s
        ScopeA i e ->
            if i >= n
                then s
                else Scope (ScopeA i (scopeShift (n - i) e))
        ScopeE fe -> Scope (ScopeE (scopeShift n <$> fe))

scopeBind :: Functor f => Int -> Scope f a -> (a -> Scope f b) -> Scope f b
scopeBind n s f =
    case unScope s of
        ScopeB b   -> Scope (ScopeB b)
        ScopeF a   -> scopeShift n (f a)
        ScopeA i e -> Scope (ScopeA i (scopeBind (n + i) e f))
        ScopeE fe  -> Scope (ScopeE ((\e -> scopeBind n e f) <$> fe))

instance Functor f => Monad (Scope f) where
    return = varScope
    (>>=) = scopeBind 0

freeVars :: Foldable f => Scope f a -> Vector a
freeVars = V.fromList . toList

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
abstractScope num ks s = scopeBind num s (maybe s (Scope . ScopeB) . flip V.elemIndex ks)

-- TODO finish!
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
