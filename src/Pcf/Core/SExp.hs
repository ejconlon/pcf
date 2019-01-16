module Pcf.Core.SExp where

import Data.Sequence (Seq)
import GHC.Generics  (Generic)

data SExp i a =
      SAtom i a
    | SList i (Seq (SExp i a))
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- instance Bifunctor SExp where

-- instance Bifoldable SExp where

-- instance Bitraversable SExp where

-- instance Lookup (SExp i) where
