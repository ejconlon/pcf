{-# LANGUAGE UndecidableInstances #-}

module Pcf.Sub where

import           Control.Monad      (ap)
import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..))
import           Data.Text          (Text)
import           Data.Vector        (Vector)
import qualified Data.Vector        as V

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

instance Functor f => Monad (Scope f) where
    return = pure
    Scope us >>= f =
        case us of
            ScopeB b   -> Scope (ScopeB b)
            ScopeF a   -> f a
            ScopeA i e -> e >>= f
            ScopeE fe  -> Scope (ScopeE ((>>= f) <$> fe))

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

class Subable f where
    abstract :: Eq a => Vector a -> f a -> f a

    abstract1 :: Eq a => a -> f a -> f a
    abstract1 k = abstract (V.singleton k)

    instantiate :: Vector (f a) -> f a -> Maybe (f a)

    instantiate1 :: f a -> f a -> Maybe (f a)
    instantiate1 v = instantiate (V.singleton v)

instance Traversable f => Subable (Scope f) where
    abstract ks = let num = V.length ks in Scope . ScopeA num . abstractScope num ks

    instantiate = instantiateScope 0

data Name n a = Name n a deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

data ExpF n a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    | Lam (Name n ()) a
    deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Exp n a = Exp { unExp :: Scope (ExpF n) a }
    deriving (Eq, Show, Functor, Foldable, Traversable, Applicative, Monad)

instance Subable (Exp n) where
    abstract ks = Exp . abstract ks . unExp
    instantiate vs = (Exp <$>) . instantiate (unExp <$> vs) . unExp

var :: a -> Exp n a
var = Exp . pure

lam :: Eq a => n -> a -> Exp n a -> Exp n a
lam n a = Exp . wrapScope . Lam (Name n ()) . unExp . abstract1 a

app :: Exp n a -> Exp n a -> Exp n a
app l r = Exp (wrapScope (App (unExp l) (unExp r)))

ifz :: Exp n a -> Exp n a -> Exp n a -> Exp n a
ifz g t e = Exp (wrapScope (Ifz (unExp g) (unExp t) (unExp e)))

zero :: Exp n a
zero = Exp (wrapScope Zero)

suc :: Exp n a -> Exp n a
suc = Exp  . wrapScope  . Suc  . unExp
