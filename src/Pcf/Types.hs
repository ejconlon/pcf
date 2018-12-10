{-# LANGUAGE TemplateHaskell #-}

module Pcf.Types where

import Bound                (Scope, makeBound, (>>>=))
import Bound.Name           (Name)
import Control.Monad        (ap)
import Data.Deriving        (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Text            (Text)
import Data.Vector          (Vector)

-- SExp

data SExp a = SAtom a | SList [SExp a] deriving (Show, Eq, Functor, Foldable, Traversable)

-- Ty

data Ty =
      Arr Ty Ty
    | Nat deriving (Show, Eq)

-- Exp

-- IDEA: Make `Ident n = Name n ()` and propagate through Exp as `Exp n a`
-- This way you're not forced into Text and can choose to Gen names
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
$(makeBound ''Exp)

-- Stmt

data Stmt a =
      Decl Text Ty
    | Defn Text (Exp a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- ExpC

type ClosC a = Vector (ExpC a)

data ExpC a =
    VarC a
  | AppC (ExpC a) (ExpC a)
  | IfzC (ExpC a) (ExpC a) (ExpC a)
  | LamC Ident Ty (ClosC a) (Scope Int ExpC a)
  | FixC Ident Ty (ClosC a) (Scope Int ExpC a)
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
    (IfzC g t e) >>= f = IfzC (g >>= f) (t >>= f) (e >>= f)
    (LamC n t c b) >>= f = LamC n t ((>>= f) <$> c) (b >>>= f)
    (FixC n t c b) >>= f = FixC n t ((>>= f) <$> c) (b >>>= f)
    (SucC e) >>= f = SucC (e >>= f)
    ZeroC >>= _ = ZeroC

-- TODO check if equality here is alpha equiv
$(deriveEq ''ExpC)
$(deriveShow ''ExpC)
$(deriveEq1 ''ExpC)
$(deriveShow1 ''ExpC)

-- ExpL

type ClosL a = Vector (ExpL a)
type AssignL a = Vector (BindL a)

data BindL a =
      RecL Ident Ty (ClosL a) (Scope Int ExpL a)
    | NRecL Ident Ty (ClosL a) (Scope Int ExpL a)
    deriving (Functor, Foldable, Traversable)

flatMapL :: (a -> ExpL b) -> BindL a -> BindL b
flatMapL f (RecL i ty c b) = RecL i ty ((>>= f) <$> c) (b >>>= f)
flatMapL f (NRecL i ty c b) = NRecL i ty ((>>= f) <$> c) (b >>>= f)

data ExpL a =
      VarL a
    | AppL (ExpL a) (ExpL a)
    | LetL (AssignL a) (Scope Int ExpL a)
    | IfzL (ExpL a) (ExpL a) (ExpL a)
    | SucL (ExpL a)
    | ZeroL
    deriving (Functor, Foldable, Traversable)

instance Applicative ExpL where
    pure = VarL
    (<*>) = ap

instance Monad ExpL where
    return = VarL
    VarL a >>= f = f a
    (AppL l r) >>= f = AppL (l >>= f) (r >>= f)
    (IfzL g t e) >>= f = IfzL (g >>= f) (t >>= f) (e >>= f)
    (SucL e) >>= f = SucL (e >>= f)
    ZeroL >>= _ = ZeroL
    LetL a b >>= f = LetL ((flatMapL f) <$> a) (b >>>= f)

$(deriveEq ''BindL)
$(deriveShow ''BindL)
$(deriveEq1 ''BindL)
$(deriveShow1 ''BindL)

$(deriveEq ''ExpL)
$(deriveShow ''ExpL)
$(deriveEq1 ''ExpL)
$(deriveShow1 ''ExpL)

-- FauxC

newtype FunId = FunId { unFunId :: Int } deriving (Eq, Show)
newtype Arity = Arity { unArity :: Int } deriving (Eq, Show, Num)
type ClosFC a = Vector (ExpFC a)
type AssignFC a = Vector (BindFC a)

data ExpFCTop a = ExpFCTop FunId Arity (Scope Int ExpC a) deriving (Functor, Foldable, Traversable)

data BindFC a =
      RecFC Ident Ty FunId (ClosFC a)
    | NRecFC Ident Ty FunId (ClosFC a)
    deriving (Functor, Foldable, Traversable)

flatMapFC :: (a -> ExpFC b) -> BindFC a -> BindFC b
flatMapFC f (RecFC i ty z c) = RecFC i ty z ((>>= f) <$> c)
flatMapFC f (NRecFC i ty z c) = NRecFC i ty z ((>>= f) <$> c)

data ExpFC a =
      VarFC a
    | AppFC (ExpFC a) (ExpFC a)
    | IfzFC (ExpFC a) (ExpFC a) (ExpFC a)
    | LetFC (AssignFC a) (Scope Int ExpFC a)
    | SucFC (ExpFC a)
    | ZeroFC
    deriving (Functor, Foldable, Traversable)

instance Applicative ExpFC where
    pure = VarFC
    (<*>) = ap

instance Monad ExpFC where
    return = VarFC
    (>>=) = undefined

$(deriveEq ''BindFC)
$(deriveShow ''BindFC)
$(deriveEq1 ''BindFC)
$(deriveShow1 ''BindFC)

$(deriveEq ''ExpFC)
$(deriveShow ''ExpFC)
$(deriveEq1 ''ExpFC)
$(deriveShow1 ''ExpFC)
