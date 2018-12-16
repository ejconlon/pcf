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

data ExpF n a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    | Lam (NameOnly n) a
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

type Exp n a = Scope (ExpF n) a

lam' :: Eq a => n -> a -> Exp n a -> Exp n a
lam' n a = wrapScope . Lam (Name n ()) . boundScope . abstract1 a

lam :: Eq n => n -> Exp n n -> Exp n n
lam n = lam' n n
