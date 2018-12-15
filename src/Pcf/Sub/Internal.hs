module Pcf.Sub.Internal where

import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Bitraversable  (Bitraversable (..))
import           GHC.Generics        (Generic)

data UnderBinder e = UnderBinder Int e
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data UnderScope f e a =
      ScopeB Int
    | ScopeF a
    | ScopeA (UnderBinder e)
    | ScopeE (f e)
    deriving (Generic, Eq, Show)

instance Functor f => Bifunctor (UnderScope f) where
    bimap _ _ (ScopeB b)   = ScopeB b
    bimap _ g (ScopeF a)   = ScopeF (g a)
    bimap f _ (ScopeA (UnderBinder i e)) = ScopeA (UnderBinder i (f e))
    bimap f _ (ScopeE fe)  = ScopeE (f <$> fe)

instance Foldable f => Bifoldable (UnderScope f) where
    bifoldr _ _ z (ScopeB _)   = z
    bifoldr _ g z (ScopeF a)   = g a z
    bifoldr f _ z (ScopeA (UnderBinder _ e)) = f e z
    bifoldr f _ z (ScopeE fe)  = foldr f z fe

instance Traversable f => Bitraversable (UnderScope f) where
    bitraverse _ _ (ScopeB b)   = pure (ScopeB b)
    bitraverse _ g (ScopeF a)   = ScopeF <$> g a
    bitraverse f _ (ScopeA (UnderBinder i e)) = ScopeA . UnderBinder i <$> f e
    bitraverse f _ (ScopeE fe)  = ScopeE <$> traverse f fe
