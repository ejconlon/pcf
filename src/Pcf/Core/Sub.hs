{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Pcf.Core.Sub where

import           Control.Monad         (ap)
import           Control.Monad.Except  (Except, MonadError (..), runExcept)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.Bifoldable       (Bifoldable (..))
import           Data.Bifunctor        (Bifunctor (..))
import           Data.Bitraversable    (Bitraversable (..))
import           Data.Foldable         (toList)
import           Data.Maybe            (fromMaybe)
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)

-- Sub

data SubError =
      ApplyError Int Int
    | UnboundError Int
    | FunctorMatchError
    | BinderMatchError
    deriving (Generic, Eq, Show)

class ThrowSub m where
    throwSub :: SubError -> m a

newtype Sub a = Sub { unSub :: Except SubError a } deriving (Generic, Functor, Applicative, Monad)

instance ThrowSub Sub where
    throwSub = Sub . throwError

runSub :: Sub a -> Either SubError a
runSub = runExcept . unSub

-- UnderBinder

data UnderBinder n e = UnderBinder { ubArity :: Int, ubInfo :: n, ubBody :: e }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- UnderScope

data UnderScope n f e a =
      ScopeB Int
    | ScopeF a
    | ScopeA (UnderBinder n e)
    | ScopeE (f e)
    deriving (Generic, Eq, Show)

instance Functor f => Bifunctor (UnderScope n f) where
    bimap _ _ (ScopeB b)                   = ScopeB b
    bimap _ g (ScopeF a)                   = ScopeF (g a)
    bimap f _ (ScopeA (UnderBinder i x e)) = ScopeA (UnderBinder i x (f e))
    bimap f _ (ScopeE fe)                  = ScopeE (f <$> fe)

instance Foldable f => Bifoldable (UnderScope n f) where
    bifoldr _ _ z (ScopeB _)                   = z
    bifoldr _ g z (ScopeF a)                   = g a z
    bifoldr f _ z (ScopeA (UnderBinder _ _ e)) = f e z
    bifoldr f _ z (ScopeE fe)                  = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
    bitraverse _ _ (ScopeB b)                   = pure (ScopeB b)
    bitraverse _ g (ScopeF a)                   = ScopeF <$> g a
    bitraverse f _ (ScopeA (UnderBinder i x e)) = ScopeA . UnderBinder i x <$> f e
    bitraverse f _ (ScopeE fe)                  = ScopeE <$> traverse f fe

-- Scope

data Scope y n f a = Scope { scopeInfo :: y, scopeBody :: UnderScope n f (Scope y n f a) a }
    deriving (Generic)

-- type BottomUp y n f g a = f (Scope n g a) -> g (Scope y n g a)

instance (Eq (f (Scope y n f a)), Eq y, Eq n, Eq a) => Eq (Scope y n f a) where
    Scope y u == Scope z v = y == z && u == v

instance (Show (f (Scope y n f a)), Show y, Show n, Show a) => Show (Scope y n f a) where
    -- TODO show info
    showsPrec d (Scope _ u) = showsPrec d u

instance Functor f => Functor (Scope y n f) where
    fmap f (Scope y us) = Scope y (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope y n f) where
    foldr f z (Scope _ us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope y n f) where
    traverse f (Scope y us) = Scope y <$> bitraverse (traverse f) f us

instance (Monoid y, Functor f) => Applicative (Scope y n f) where
    pure = freeVarScope
    (<*>) = ap

subScopeShift :: Functor f => Int -> Int -> Scope y n f a -> Scope y n f a
subScopeShift c d s@(Scope y us) =
    case us of
        ScopeB b ->
            if b < c
                then s
                else Scope y (ScopeB (b + d))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope y (ScopeA (UnderBinder i x (subScopeShift (c + i) d e)))
        ScopeE fe -> Scope y (ScopeE (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope y n f a -> Scope y n f a
scopeShift = subScopeShift 0

-- TODO inject behavior here? What's correct default for bind? Last monoid?
scopeBind :: Functor f => Int -> Scope y n f a -> (a -> Scope y n f b) -> Scope y n f b
scopeBind n (Scope y us) f =
    case us of
        ScopeB b                   -> Scope y (ScopeB b)
        ScopeF a                   -> scopeShift n (f a)
        ScopeA (UnderBinder i x e) -> Scope y (ScopeA (UnderBinder i x (scopeBind (n + i) e f)))
        ScopeE fe                  -> Scope y (ScopeE ((\e -> scopeBind n e f) <$> fe))

scopeBindOpt :: Functor f => Int -> Scope y n f a -> (a -> Maybe (Scope y n f a)) -> Scope y n f a
scopeBindOpt n s@(Scope y us) f =
    case us of
        ScopeB b                   -> s
        ScopeF a                   ->
            case f a of
                Nothing -> s
                Just s' -> scopeShift n s'
        ScopeA (UnderBinder i x e) -> Scope y (ScopeA (UnderBinder i x (scopeBindOpt (n + i) e f)))
        ScopeE fe                  -> Scope y (ScopeE ((\e -> scopeBindOpt n e f) <$> fe))

instance (Monoid y, Functor f) => Monad (Scope y n f) where
    return = freeVarScope
    (>>=) = scopeBind 0

instance Monoid y => MonadTrans (Scope y n) where
    lift = liftScope

boundVarScope :: Monoid y => Int -> Scope y n f a
boundVarScope = boundVarScope' mempty

boundVarScope' :: y -> Int -> Scope y n f a
boundVarScope' y = Scope y . ScopeB

freeVarScope :: Monoid y => a -> Scope y n f a
freeVarScope = freeVarScope' mempty

freeVarScope' :: y -> a -> Scope y n f a
freeVarScope' y = Scope y . ScopeF

wrapScope :: Monoid y => f (Scope y n f a) -> Scope y n f a
wrapScope = wrapScope' mempty

wrapScope' :: y -> f (Scope y n f a) -> Scope y n f a
wrapScope' y = Scope y . ScopeE

liftScope :: (Monoid y, Functor f) => f a -> Scope y n f a
liftScope = liftScope' mempty

-- NOTE same annotation is used at all levels!
liftScope' :: Functor f => y -> f a -> Scope y n f a
liftScope' y = wrapScope' y . (freeVarScope' y <$>)

binderScope :: Monoid y => Binder y n f a -> Scope y n f a
binderScope = binderScope' mempty

binderScope' :: y -> Binder y n f a -> Scope y n f a
binderScope' y = Scope y . ScopeA . unBinder

scopeFreeVars :: (Foldable f, Ord a) => Scope y n f a -> Set a
scopeFreeVars = S.fromList . toList

-- scopeMapInfo :: Functor f => (n -> o) -> Scope n f a -> Scope o f a
-- scopeMapInfo f s =
--     case unScope s of
--         ScopeB b  -> Scope (ScopeB b)
--         ScopeF a  -> Scope (ScopeF a)
--         ScopeA b  -> Scope (ScopeA (unBinder (binderMapInfo f (Binder b))))
--         ScopeE fe -> Scope (ScopeE (scopeMapInfo f <$> fe))

-- scopeTraverseInfo :: (Traversable f, Applicative m) => (n -> m o) -> Scope n f a -> m (Scope o f a)
-- scopeTraverseInfo f s =
--     case unScope s of
--         ScopeB b  -> pure (Scope (ScopeB b))
--         ScopeF a  -> pure (Scope (ScopeF a))
--         ScopeA e  -> Scope . ScopeA . unBinder <$> binderTraverseInfo f (Binder e)
--         ScopeE fe -> Scope . ScopeE <$> traverse (scopeTraverseInfo f) fe

matchFunctor :: Scope y n f a -> Maybe (f (Scope y n f a))
matchFunctor (Scope y (ScopeE fe)) = pure fe
matchFunctor _                   = Nothing

forceFunctor :: (ThrowSub m, Applicative m) => Scope y n f a -> m (f (Scope y n f a))
forceFunctor s =
    case matchFunctor s of
        Just x  -> pure x
        Nothing -> throwSub FunctorMatchError

-- transformScope :: Functor f => BottomUp n f g a -> Scope n f a -> Scope n g a
-- transformScope t s =
--     case unScope s of
--         ScopeB b  -> Scope (ScopeB b)
--         ScopeF a  -> Scope (ScopeF a)
--         ScopeA e  -> Scope (ScopeA (unBinder (transformBinder t (Binder e))))
--         ScopeE fe -> Scope (ScopeE (t (transformScope t <$> fe)))

-- Binder

newtype Binder y n f a = Binder { unBinder :: UnderBinder n (Scope y n f a) }
    deriving (Generic, Functor, Foldable, Traversable)

instance (Eq (f (Scope y n f a)), Eq y, Eq n, Eq a) => Eq (Binder y n f a) where
    Binder u == Binder v = u == v

instance (Show (f (Scope y n f a)), Show y, Show n, Show a) => Show (Binder y n f a) where
    showsPrec d (Binder u) = showsPrec d u

matchBinder :: Scope y n f a -> Maybe (Binder y n f a)
matchBinder (Scope _ (ScopeA ub)) = pure (Binder ub)
matchBinder _                   = Nothing

forceBinder :: (ThrowSub m, Applicative m) => Scope y n f a -> m (Binder y n f a)
forceBinder s =
    case matchBinder s of
        Just x  -> pure x
        Nothing -> throwSub BinderMatchError

binderArity :: Binder y n f a -> Int
binderArity = ubArity . unBinder

binderInfo :: Binder y n f a -> n
binderInfo = ubInfo . unBinder

binderBody :: Binder y n f a -> Scope y n f a
binderBody = ubBody . unBinder

binderFreeVars :: (Foldable f, Ord a) => Binder y n f a -> Set a
binderFreeVars = scopeFreeVars . binderBody

-- binderMapInfo :: Functor f => (n -> o) -> Binder n f a -> Binder o f a
-- binderMapInfo f (Binder (UnderBinder i x b)) = Binder (UnderBinder i (f x) (scopeMapInfo f b))

-- binderTraverseInfo :: (Traversable f, Applicative m) => (n -> m o) -> Binder n f a -> m (Binder o f a)
-- binderTraverseInfo f (Binder (UnderBinder i x b)) = (\y c -> Binder (UnderBinder i y c)) <$> f x <*> scopeTraverseInfo f b

-- transformBinder :: Functor f => BottomUp n f g a -> Binder n f a -> Binder n g a
-- transformBinder t (Binder (UnderBinder i x b)) = Binder (UnderBinder i x (transformScope t b))

-- Abstraction and instantiation

-- TODO allow insertion of info on bind?
subAbstract :: (Monoid y, Functor f, Eq a) => Int -> n -> Vector a -> Scope y n f a -> Binder y n f a
subAbstract n x ks s = Binder (UnderBinder n x (scopeBindOpt 0 s ((boundVarScope <$>) . flip V.elemIndex ks)))

-- TODO combine info on sub?
subInstantiate :: Functor f => Int -> Vector (Scope y n f a) -> Scope y n f a -> Scope y n f a
subInstantiate n vs s@(Scope y us) =
    case us of
        ScopeB b -> fromMaybe s (vs V.!? (b - n))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope y (ScopeA (UnderBinder i x (subInstantiate (n + i) (scopeShift i <$> vs) e)))
        ScopeE fe -> Scope y (ScopeE (subInstantiate n vs <$> fe))

abstract :: (Monoid y, Functor f, Eq a) => n -> Vector a -> Scope y n f a -> Binder y n f a
abstract x ks = let n = V.length ks in subAbstract n x ks . scopeShift n

instantiate :: Functor f => Vector (Scope y n f a) -> Scope y n f a -> Scope y n f a
instantiate = subInstantiate 0

rawApply :: (ThrowSub m, Applicative m, Functor f) => Vector (Scope y n f a) -> Int -> Scope y n f a -> m (Scope y n f a)
rawApply vs i e =
    let len = V.length vs
    in if len == i
        then pure (scopeShift (-1) (instantiate vs e))
        else throwSub (ApplyError len i)

apply :: (ThrowSub m, Applicative m, Functor f)  => Vector (Scope y n f a) -> Binder y n f a -> m (Scope y n f a)
apply vs (Binder (UnderBinder i _ e)) = rawApply vs i e

abstract1 :: (Monoid y, Functor f, Eq a) => n -> a -> Scope y n f a -> Binder y n f a
abstract1 n k = abstract n (V.singleton k)

instantiate1 :: Functor f => Scope y n f a -> Scope y n f a -> Scope y n f a
instantiate1 v = instantiate (V.singleton v)

apply1 :: (ThrowSub m, Applicative m, Functor f) => Scope y n f a -> Binder y n f a -> m (Scope y n f a)
apply1 v = apply (V.singleton v)

-- ScopeFold

data ScopeFold y n f a r = ScopeFold
    { sfBound   :: y -> Int -> r
    , sfFree    :: y -> a -> r
    , sfBinder  :: y -> Binder y n f a -> r
    , sfFunctor :: y -> f (Scope y n f a) -> r
    } deriving (Generic, Functor)

contramapFold :: (x -> y) -> ScopeFold y n f a r -> ScopeFold x n f a r
contramapFold f (ScopeFold bound free binder functor) = undefined

-- transformFold :: Functor f => BottomUp n f g a -> ScopeFold n g a r -> ScopeFold n f a r
-- transformFold t (ScopeFold bound free binder functor) = ScopeFold bound free (binder . transformBinder t) (functor . t . (transformScope t <$>))

boundFold :: ThrowSub m => (y -> a -> m r) -> (y -> Binder y n f a -> m r) -> (y -> f (Scope y n f a) -> m r) -> ScopeFold y n f a (m r)
boundFold = ScopeFold (const (throwSub . UnboundError))

foldScope :: Functor f => ScopeFold y n f a r -> Scope y n f a -> r
foldScope (ScopeFold bound free binder functor) (Scope y us) =
    case us of
        ScopeB b  -> bound y b
        ScopeF a  -> free y a
        ScopeA ub -> binder y (Binder ub)
        ScopeE fe -> functor y fe

-- Name

data Name n a = Name { nameKey :: n, nameValue :: a } deriving (Generic, Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
