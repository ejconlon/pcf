{-# LANGUAGE UndecidableInstances #-}
module Pcf.Core.Sub (
    Binder (..),
    Name (..),
    NameOnly,
    Scope (..),
    ScopeFold (..),
    abstract,
    abstract1,
    apply,
    apply1,
    binderArity,
    binderBody,
    binderFreeVars,
    binderInfo,
    binderMapInfo,
    binderTraverseInfo,
    boundScope,
    foldScope,
    instantiate,
    instantiate1,
    liftScope,
    matchBinder,
    scopeFreeVars,
    scopeMapInfo,
    scopeTraverseInfo,
    varScope,
    wrapScope
) where

import           Control.Monad          (ap)
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.Trans    (MonadTrans (..))
import           Data.Bifoldable        (bifoldr)
import           Data.Bifunctor         (bimap)
import           Data.Bitraversable     (bitraverse)
import           Data.Foldable          (toList)
import           Data.Maybe             (fromMaybe)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import           Pcf.Core.Sub.Internal

-- Scope

newtype Scope n f a = Scope { unScope :: UnderScope n f (Scope n f a) a }
    deriving (Generic)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
    Scope u == Scope v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
    showsPrec d (Scope u) = showsPrec d u

instance Functor f => Functor (Scope n f) where
    fmap f (Scope us) = Scope (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope n f) where
    foldr f z (Scope us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope n f) where
    traverse f (Scope us) = Scope <$> bitraverse (traverse f) f us

instance Functor f => Applicative (Scope n f) where
    pure = varScope
    (<*>) = ap

subScopeShift :: Functor f => Int -> Int -> Scope n f a -> Scope n f a
subScopeShift c d s@(Scope us) =
    case us of
        ScopeB b ->
            if b < c
                then s
                else Scope (ScopeB (b + d))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (subScopeShift (c + i) d e)))
        ScopeE fe -> Scope (ScopeE (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope n f a -> Scope n f a
scopeShift = subScopeShift 0

scopeBind :: Functor f => Int -> Scope n f a -> (a -> Scope n f b) -> Scope n f b
scopeBind n s f =
    case unScope s of
        ScopeB b                   -> Scope (ScopeB b)
        ScopeF a                   -> scopeShift n (f a)
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (scopeBind (n + i) e f)))
        ScopeE fe                  -> Scope (ScopeE ((\e -> scopeBind n e f) <$> fe))

scopeBindOpt :: Functor f => Int -> Scope n f a -> (a -> Maybe (Scope n f a)) -> Scope n f a
scopeBindOpt n s f = scopeBind n s (\a -> fromMaybe (varScope a) (f a))

instance Functor f => Monad (Scope n f) where
    return = varScope
    (>>=) = scopeBind 0

instance MonadTrans (Scope n) where
    lift = liftScope

varScope :: a -> Scope n f a
varScope = Scope . ScopeF

wrapScope :: f (Scope n f a) -> Scope n f a
wrapScope = Scope . ScopeE

liftScope :: Functor f => f a -> Scope n f a
liftScope = wrapScope . (pure <$>)

boundScope :: Binder n f a -> Scope n f a
boundScope = Scope . ScopeA . unBinder

scopeFreeVars :: Foldable f => Scope n f a -> Vector a
scopeFreeVars = V.fromList . toList

scopeMapInfo :: Functor f => (n -> o) -> Scope n f a -> Scope o f a
scopeMapInfo f s =
    case unScope s of
        ScopeB b  -> Scope (ScopeB b)
        ScopeF a  -> Scope (ScopeF a)
        ScopeA b  -> Scope (ScopeA (unBinder (binderMapInfo f (Binder b))))
        ScopeE fe -> Scope (ScopeE (scopeMapInfo f <$> fe))

scopeTraverseInfo :: (Traversable f, Applicative m) => (n -> m o) -> Scope n f a -> m (Scope o f a)
scopeTraverseInfo f s =
    case unScope s of
        ScopeB b  -> pure (Scope (ScopeB b))
        ScopeF a  -> pure (Scope (ScopeF a))
        ScopeA b  -> Scope . ScopeA . unBinder <$> binderTraverseInfo f (Binder b)
        ScopeE fe -> Scope . ScopeE <$> traverse (scopeTraverseInfo f) fe

-- Binder

newtype Binder n f a = Binder { unBinder :: UnderBinder n (Scope n f a) }
    deriving (Generic, Functor, Foldable, Traversable)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Binder n f a) where
    Binder u == Binder v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Binder n f a) where
    showsPrec d (Binder u) = showsPrec d u

matchBinder :: Scope n f a -> Maybe (Binder n f a)
matchBinder (Scope (ScopeA ub)) = pure (Binder ub)
matchBinder _                   = Nothing

binderArity :: Binder n f a -> Int
binderArity (Binder (UnderBinder a _ _)) = a

binderInfo :: Binder n f a -> n
binderInfo (Binder (UnderBinder _ x _)) = x

binderBody :: Binder n f a -> Scope n f a
binderBody (Binder (UnderBinder _ _ b)) = b

binderFreeVars :: Foldable f => Binder n f a -> Vector a
binderFreeVars = scopeFreeVars . binderBody

binderMapInfo :: Functor f => (n -> o) -> Binder n f a -> Binder o f a
binderMapInfo f (Binder (UnderBinder i x b)) = Binder (UnderBinder i (f x) (scopeMapInfo f b))

binderTraverseInfo :: (Traversable f, Applicative m) => (n -> m o) -> Binder n f a -> m (Binder o f a)
binderTraverseInfo f (Binder (UnderBinder i x b)) = (\y c -> Binder (UnderBinder i y c)) <$> f x <*> scopeTraverseInfo f b

-- Abstraction and instantiation

subAbstract :: (Functor f, Eq a) => Int -> n -> Vector a -> Scope n f a -> Binder n f a
subAbstract n x ks s = Binder (UnderBinder n x (scopeBindOpt 0 s ((Scope . ScopeB <$>) . flip V.elemIndex ks)))

subInstantiate :: Functor f => Int -> Vector (Scope n f a) -> Scope n f a -> Scope n f a
subInstantiate n vs s =
    case unScope s of
        ScopeB b -> fromMaybe s (vs V.!? (b - n))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (subInstantiate (n + i) (scopeShift i <$> vs) e)))
        ScopeE fe -> Scope (ScopeE (subInstantiate n vs <$> fe))

abstract :: (Functor f, Eq a) => n -> Vector a -> Scope n f a -> Binder n f a
abstract x ks = let n = V.length ks in subAbstract n x ks . scopeShift n

instantiate :: Functor f => Vector (Scope n f a) -> Scope n f a -> Scope n f a
instantiate = subInstantiate 0

rawApply :: Functor f => Vector (Scope n f a) -> Int -> Scope n f a -> Maybe (Scope n f a)
rawApply vs i e =
    if V.length vs == i
        then pure (scopeShift (-1) (instantiate vs e))
        else Nothing

apply :: Functor f => Vector (Scope n f a) -> Binder n f a -> Maybe (Scope n f a)
apply vs (Binder (UnderBinder i _ e)) = rawApply vs i e

abstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Binder n f a
abstract1 n k = abstract n (V.singleton k)

instantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
instantiate1 v = instantiate (V.singleton v)

apply1 :: Functor f => Scope n f a -> Binder n f a -> Maybe (Scope n f a)
apply1 v = apply (V.singleton v)

-- ScopeFold

data ScopeFold n f a r = ScopeFold
    { sfBound   :: Int -> r
    , sfFree    :: a -> r
    , sfBinder  :: Binder n f a -> r
    , sfFunctor :: f (Scope n f a) -> r
    } deriving (Generic, Functor)

foldScope :: Functor f => ScopeFold n f a r -> Scope n f a -> r
foldScope (ScopeFold bound free binder functor) s =
    case unScope s of
        ScopeB b  -> bound b
        ScopeF a  -> free a
        ScopeA ub -> binder (Binder ub)
        ScopeE fe -> functor fe

-- Name

data Name n a = Name n a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
