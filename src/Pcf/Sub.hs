{-# LANGUAGE UndecidableInstances #-}

module Pcf.Sub where

import           Control.Monad       (ap)
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Bitraversable  (Bitraversable (..))
import           Data.Foldable       (toList)
import           Data.Maybe          (fromMaybe)
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

subScopeShift :: Functor f => Int -> Int -> Scope f a -> Scope f a
subScopeShift c d s@(Scope us) =
    case us of
        ScopeB b ->
            if b < c
                then s
                else Scope (ScopeB (b + d))
        ScopeF _ -> s
        ScopeA i e -> Scope (ScopeA i (subScopeShift (c + i) d e))
        ScopeE fe -> Scope (ScopeE (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope f a -> Scope f a
scopeShift d s = subScopeShift 0 d s

scopeBind :: Functor f => Int -> Scope f a -> (a -> Scope f b) -> Scope f b
scopeBind n s f =
    case unScope s of
        ScopeB b   -> Scope (ScopeB b)
        ScopeF a   -> scopeShift n (f a)
        ScopeA i e -> Scope (ScopeA i (scopeBind (n + i) e f))
        ScopeE fe  -> Scope (ScopeE ((\e -> scopeBind n e f) <$> fe))

scopeBindOpt :: Functor f => Int -> Scope f a -> (a -> Maybe (Scope f a)) -> Scope f a
scopeBindOpt n s f = scopeBind n s (\a -> fromMaybe (varScope a) (f a))

instance Functor f => Monad (Scope f) where
    return = varScope
    (>>=) = scopeBind 0

freeVars :: Foldable f => Scope f a -> Vector a
freeVars = V.fromList . toList

subAbstract :: (Functor f, Eq a) => Vector a -> Scope f a -> Scope f a
subAbstract ks s = scopeBindOpt 0 s ((Scope . ScopeB <$>) . flip V.elemIndex ks)

subInstantiate :: Functor f => Int -> Vector (Scope f a) -> Scope f a -> Scope f a
subInstantiate n vs s =
    case unScope s of
        ScopeB b -> fromMaybe s (vs V.!? (b - n))
        ScopeF _ -> s
        ScopeA i e -> subInstantiate (n + i) (scopeShift i <$> vs) e
        ScopeE fe -> Scope (ScopeE (subInstantiate n vs <$> fe))

abstract :: (Functor f, Eq a) => Vector a -> Scope f a -> Scope f a
abstract ks = let num = V.length ks in Scope . ScopeA num . subAbstract ks . scopeShift num

instantiate :: Functor f => Vector (Scope f a) -> Scope f a -> Scope f a
instantiate = subInstantiate 0

abstract1 :: (Functor f, Eq a) => a -> Scope f a -> Scope f a
abstract1 k = abstract (V.singleton k)

instantiate1 :: Functor f => Scope f a -> Scope f a -> Scope f a
instantiate1 v = instantiate (V.singleton v)

-- Name

data Name n a = Name n a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
