{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.Types where

import Bound               (Scope)
import Control.Monad       (ap)
import Data.Deriving       (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Sequence       (Seq)
import Data.Text           (Text)
import Pcf.Core.BoundCrazy (Munge (..), mungeBind)

type Name = Text

data Type0 =
      TyBool0
    | TyArr0 (Seq Type0) Type0
    | TyCont0 Type0
    deriving (Eq, Show)

data Exp0 a =
      Var0 a
    | Call0 (Exp0 a) (Seq (Exp0 a))
    | Lam0 (Seq (Name, Type0)) (Scope Int Exp0 a)
    | Bool0 Bool
    | If0 (Exp0 a) (Exp0 a) (Exp0 a)
    | Control0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    deriving (Functor, Foldable, Traversable)

data Dir0 =
      DirCallFun0
    | DirCallArg0 Int
    | DirLamBody0
    | DirIfGuard0
    | DirIfThen0
    | DirIfElse0
    | DirControlBody0
    | DirThrowFun0
    | DirThrowArg0
    deriving (Eq, Show)

type Path0 = Seq Dir0

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
