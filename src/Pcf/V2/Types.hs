module Pcf.V2.Types where

import Data.Vector  (Vector)
import GHC.Generics (Generic)
import Pcf.Core.Sub
-- import Pcf.V2.Info

-- SExp

data SExp i a =
      SAtom !i !a
    | SList !i !(Vector (SExp i a))
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- Ty

data Ty =
      Arr !Ty !Ty
    | Nat deriving (Generic, Eq, Show)

-- Exp

data ExpF a =
      App !a !a
    | Ifz !a !a !a
    | Zero
    | Suc !a
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data ExpN n = ExpN { expName :: !(NameOnly n), expTy :: !Ty } deriving (Generic, Eq, Show)

type Exp n a = Scope (ExpN n) ExpF a

-- NOTE: Is this the best way to compose?
-- data InfoF a = Info Int a deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
-- type F = Compose InfoF ExpF
-- type N = Exp n
-- type Info n a = Scope (N n) F a

-- Stmt

data Stmt n a =
      Decl !n !Ty
    | Defn !n !(Exp n a)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- ExpC

data ExpCF a =
      AppC !a !a
    | IfzC !a !a !a
    | SucC !a
    | ZeroC
    | ClosC !(Vector a) !a
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

type ExpC n a = Scope (ExpN n) ExpCF a
