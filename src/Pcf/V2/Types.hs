module Pcf.V2.Types where

import GHC.Generics (Generic)
import Pcf.Core.Sub

-- SExp

data SExp a = SAtom a | SList [SExp a] deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- Ty

data Ty =
      Arr Ty Ty
    | Nat deriving (Generic, Eq, Show)

-- Exp

data ExpF a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data ExpN n = ExpN (NameOnly n) Ty deriving (Generic, Eq, Show)

type Exp n a = Scope (ExpN n) ExpF a
