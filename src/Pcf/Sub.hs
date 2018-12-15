{-# LANGUAGE UndecidableInstances #-}
module Pcf.Sub (
    Binder (..),
    Name (..),
    NameOnly,
    Scope (..),
    abstract,
    abstract1,
    binderArity,
    binderBody,
    boundScope,
    freeVars,
    -- instantiate,
    -- instantiate1
    liftScope,
    matchBinder,
    varScope,
    wrapScope
) where

import           Control.Monad       (ap)
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Bifoldable     (bifoldr)
import           Data.Bifunctor      (bimap)
import           Data.Bitraversable  (bitraverse)
import           Data.Foldable       (toList)
import           Data.Maybe          (fromMaybe)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import           Pcf.Sub.Internal

-- Scope

newtype Scope f a = Scope { unScope :: UnderScope f (Scope f a) a }
    deriving (Generic)

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

subScopeShift :: Functor f => Int -> Int -> Scope f a -> Scope f a
subScopeShift c d s@(Scope us) =
    case us of
        ScopeB b ->
            if b < c
                then s
                else Scope (ScopeB (b + d))
        ScopeF _ -> s
        ScopeA (UnderBinder i e) -> Scope (ScopeA (UnderBinder i (subScopeShift (c + i) d e)))
        ScopeE fe -> Scope (ScopeE (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope f a -> Scope f a
scopeShift d s = subScopeShift 0 d s

scopeBind :: Functor f => Int -> Scope f a -> (a -> Scope f b) -> Scope f b
scopeBind n s f =
    case unScope s of
        ScopeB b   -> Scope (ScopeB b)
        ScopeF a   -> scopeShift n (f a)
        ScopeA (UnderBinder i e) -> Scope (ScopeA (UnderBinder i (scopeBind (n + i) e f)))
        ScopeE fe  -> Scope (ScopeE ((\e -> scopeBind n e f) <$> fe))

scopeBindOpt :: Functor f => Int -> Scope f a -> (a -> Maybe (Scope f a)) -> Scope f a
scopeBindOpt n s f = scopeBind n s (\a -> fromMaybe (varScope a) (f a))

instance Functor f => Monad (Scope f) where
    return = varScope
    (>>=) = scopeBind 0

instance MonadTrans Scope where
    lift = liftScope

varScope :: a -> Scope f a
varScope = Scope . ScopeF

wrapScope :: f (Scope f a) -> Scope f a
wrapScope = Scope . ScopeE

liftScope :: Functor f => f a -> Scope f a
liftScope = wrapScope . (pure <$>)

boundScope :: Binder f a -> Scope f a
boundScope = Scope . ScopeA . unBinder

freeVars :: Foldable f => Scope f a -> Vector a
freeVars = V.fromList . toList

-- Binder

newtype Binder f a = Binder { unBinder :: UnderBinder (Scope f a) }
    deriving (Generic, Functor, Foldable, Traversable)

instance (Eq (f (Scope f a)), Eq a) => Eq (Binder f a) where
    Binder u == Binder v = u == v

instance (Show (f (Scope f a)), Show a) => Show (Binder f a) where
    showsPrec d (Binder u) = showsPrec d u

matchBinder :: Scope f a -> Maybe (Binder f a)
matchBinder (Scope (ScopeA ub)) = pure (Binder ub)
matchBinder _ = Nothing

binderArity :: Binder f a -> Int
binderArity (Binder (UnderBinder a _)) = a

binderBody :: Binder f a -> Scope f a
binderBody (Binder (UnderBinder _ b)) = b

-- Abstraction and instantiation

subAbstract :: (Functor f, Eq a) => Int -> Vector a -> Scope f a -> Binder f a
subAbstract n ks s = Binder (UnderBinder n (scopeBindOpt 0 s ((Scope . ScopeB <$>) . flip V.elemIndex ks)))

-- subInstantiate :: Functor f => Int -> Vector (Scope f a) -> Scope f a -> Scope f a
-- subInstantiate n vs s =
--     case unScope s of
--         ScopeB b -> fromMaybe s (vs V.!? (b - n))
--         ScopeF _ -> s
--         ScopeA i e -> subInstantiate (n + i) (scopeShift i <$> vs) e
--         ScopeE fe -> Scope (ScopeE (subInstantiate n vs <$> fe))

abstract :: (Functor f, Eq a) => Vector a -> Scope f a -> Binder f a
abstract ks = let n = V.length ks in subAbstract n ks . scopeShift n

-- instantiate :: Functor f => Vector (Scope f a) -> Binder f a -> Scope f a
-- instantiate = subInstantiate 0

abstract1 :: (Functor f, Eq a) => a -> Scope f a -> Binder f a
abstract1 k = abstract (V.singleton k)

-- instantiate1 :: Functor f => Scope f a -> Binder f a -> Scope f a
-- instantiate1 v = instantiate (V.singleton v)

-- Name

data Name n a = Name n a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
