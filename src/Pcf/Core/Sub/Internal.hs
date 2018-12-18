module Pcf.Core.Sub.Internal where

import Data.Bifoldable    (Bifoldable (..))
import Data.Bifunctor     (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import GHC.Generics       (Generic)

data UnderBinder n e = UnderBinder { ubArity :: !Int, ubInfo :: !n, ubBody :: !e }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data UnderScope n f e a =
      ScopeB !Int
    | ScopeF !a
    | ScopeA !(UnderBinder n e)
    | ScopeE !(f e)
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
