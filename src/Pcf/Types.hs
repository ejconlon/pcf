{-# LANGUAGE TemplateHaskell #-}

module Pcf.Types where

import Bound (Scope, (>>>=))
import Control.Monad (ap)
import Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Vector (Vector)
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

type Clos a = Vector (ExpC a)

data ExpC a =
    VarC a
  | AppC (ExpC a) (ExpC a)
  | IfzC (ExpC a) (ExpC a) (Scope () ExpC a)
  | LamC Ty (Clos a) (Scope () ExpC a)
  | FixC Ty (Clos a) (Scope () ExpC a)
  | SucC (ExpC a)
  | ZeroC
  deriving (Functor, Foldable, Traversable)

instance Applicative ExpC where
    pure = VarC
    (<*>) = ap

instance Monad ExpC where
    return = VarC
    VarC a >>= f = f a
    (AppC l r) >>= f = AppC (l >>= f) (r >>= f)
    (IfzC i t e) >>= f = IfzC (i >>= f) (t >>= f) (e >>>= f)
    (LamC t c b) >>= f = LamC t ((>>= f) <$> c) (b >>>= f)
    (FixC t c b) >>= f = FixC t ((>>= f) <$> c) (b >>>= f)
    (SucC e) >>= f = SucC (e >>= f)
    ZeroC >>= _ = ZeroC

$(deriveEq ''ExpC)
$(deriveShow ''ExpC)
$(deriveEq1 ''ExpC)
$(deriveShow1 ''ExpC)
