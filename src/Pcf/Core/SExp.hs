module Pcf.Core.SExp where

import Data.Vector  (Vector)
import GHC.Generics (Generic)

data SExp i a =
      SAtom i a
    | SList i (Vector (SExp i a))
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- instance Bifunctor SExp where

-- instance Bifoldable SExp where

-- instance Bitraversable SExp where

-- instance Lookup (SExp i) where
