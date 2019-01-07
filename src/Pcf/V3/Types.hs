{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.Types where

import Bound               (Scope)
import Control.Monad       (ap)
import Data.Deriving       (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Text           (Text)
import Data.Vector         (Vector)
import Pcf.Core.BoundCrazy (Munge (..), mungeBind)

type Name = Text

data Type0 =
      TyBool0
    | TyArr0 (Vector Type0) Type0
    | TyCont0 Type0
    deriving (Eq, Show)

data Exp0 a =
      Var0 a
    | Call0 (Exp0 a) (Vector (Exp0 a))
    | Lam0 (Vector (Name, Type0)) (Scope Int Exp0 a)
    | Bool0 Bool
    | If0 (Exp0 a) (Exp0 a) (Exp0 a)
    | Control0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    deriving (Functor, Foldable, Traversable)

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    (>>=) = mungeBind

instance Munge Exp0 where
    munge = undefined -- TODO define this...

$(deriveEq ''Exp0)
$(deriveShow ''Exp0)
$(deriveEq1 ''Exp0)
$(deriveShow1 ''Exp0)
