module Pcf.Types where

import Bound (Scope, (>>>=))
import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Control.Monad.Gen (Gen)
import Control.Monad.Trans.Maybe (MaybeT)

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
