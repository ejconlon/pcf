{-# LANGUAGE TemplateHaskell #-}

module Pcf.Types where

import Bound                (Bound (..), Scope, (>>>=))
import Bound.Name           (Name)
import Control.Monad        (ap)
import Data.Deriving        (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Text            (Text)
import Data.Vector          (Vector)

data SExp a = SAtom a | SList [SExp a] deriving (Show, Eq, Functor, Foldable, Traversable)

data Ty =
      Arr Ty Ty
    | Nat deriving (Show, Eq)

type Ident = Name Text ()

data Exp a =
      Var a
    | App (Exp a) (Exp a)
    | Ifz (Exp a) (Exp a) (Exp a)
    | Lam Ident Ty (Scope () Exp a)
    | Fix Ident Ty (Scope () Exp a)
    | Suc (Exp a)
    | Zero
    deriving (Functor, Foldable, Traversable)

-- NOTE equality here should be alpha equivalence
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
    (Ifz g t e) >>= f = Ifz (g >>= f) (t >>= f) (e >>= f)
    (Lam n t b) >>= f = Lam n t (b >>>= f)
    (Fix n t b) >>= f = Fix n t (b >>>= f)
    (Suc e) >>= f = Suc (e >>= f)
    Zero >>= _ = Zero

data Stmt a =
      Decl Text Ty
    | Defn Text (Exp a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- type Clos = Vector Text

-- data ExpC a =
--     VarC a
--   | AppC (ExpC a) (ExpC a)
--   | IfzC (ExpC a) (ExpC a) (ExpC a)
--   | LamC Ident Ty Clos (Scope () ExpC a)
--   | FixC Ident Ty Clos (Scope () ExpC a)
--   | SucC (ExpC a)
--   | ZeroC
--   deriving (Functor, Foldable, Traversable)

-- instance Applicative ExpC where
--     pure = VarC
--     (<*>) = ap

-- instance Monad ExpC where
--     return = VarC
--     VarC a >>= f = f a
--     (AppC l r) >>= f = AppC (l >>= f) (r >>= f)
--     (IfzC g t e) >>= f = IfzC (g >>= f) (t >>= f) (e >>= f)
--     (LamC n t c b) >>= f = LamC n t c (b >>>= f)
--     (FixC n t c b) >>= f = FixC n t c (b >>>= f)
--     (SucC e) >>= f = SucC (e >>= f)
--     ZeroC >>= _ = ZeroC

-- -- NOTE equality here is not alpha-equivalence
-- $(deriveEq ''ExpC)
-- $(deriveShow ''ExpC)
-- $(deriveEq1 ''ExpC)
-- $(deriveShow1 ''ExpC)
