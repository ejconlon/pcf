{-# LANGUAGE TemplateHaskell #-}

module Pcf.Types where

import Bound (Scope, (>>>=))
import Control.Monad (ap)
import Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Control.Monad.Gen (Gen, runGen)
import Control.Monad.Trans.Maybe (MaybeT(..))

data Ty =
      Arr Ty Ty
    | Nat deriving (Show, Eq)

data Exp a =
      Var a
    | App (Exp a) (Exp a)
    | Ifz (Exp a) (Exp a) (Scope () Exp a)
    | Lam Ty (Scope () Exp a)
    | Fix Ty (Scope () Exp a)
    | Suc (Exp a)
    | Zero
    deriving (Functor, Foldable, Traversable)

$(deriveEq ''Exp)
$(deriveShow ''Exp)
$(deriveEq1 ''Exp)
$(deriveShow1 ''Exp)

instance Applicative Exp where
    pure = Var
    (<*>) = ap

instance Monad Exp where
    return = Var
    Var a >>= f = f a
    (App l r) >>= f = App (l >>= f) (r >>= f)
    (Ifz i t e) >>= f = Ifz (i >>= f) (t >>= f) (e >>>= f)
    (Lam t b) >>= f = Lam t (b >>>= f)
    (Fix t b) >>= f = Fix t (b >>>= f)
    (Suc e) >>= f = Suc (e >>= f)
    Zero >>= _ = Zero

type TyM a = MaybeT (Gen a)

runTyM :: Enum a => TyM a b -> Maybe b
runTyM = runGen . runMaybeT
