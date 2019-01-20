{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.Types where

import           Bound         (Scope, abstract, abstract1, (>>>=))
import           Control.Monad (ap)
import           Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

type Name = Text

-- Type0

data Type0 =
      TyBool0
    | TyArr0 (Seq Type0) Type0
    | TyCont0 Type0
    deriving (Generic, Eq, Show)

-- Exp0

data Exp0 a =
      Var0 a
    | Call0 (Exp0 a) (Seq (Exp0 a))
    | Lam0 (Seq (Name, Type0)) (Scope Int Exp0 a)
    | Bool0 Bool
    | If0 (Exp0 a) (Exp0 a) (Exp0 a)
    | CallCC0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    deriving (Generic, Functor, Foldable, Traversable)

data Dir0 =
      DirCallFun0
    | DirCallArg0 Int
    | DirLamBody0
    | DirIfGuard0
    | DirIfThen0
    | DirIfElse0
    | DirCallCCBody0
    | DirThrowFun0
    | DirThrowArg0
    deriving (Eq, Show)

type Path0 = Seq Dir0

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    m >>= f =
        case m of
            Var0 a         -> f a
            Call0 e xs     -> Call0 (e >>= f) ((>>= f) <$> xs)
            Lam0 nts b     -> Lam0 nts (b >>>= f)
            Bool0 b        -> Bool0 b
            If0 g t e      -> If0 (g >>= f) (t >>= f) (e >>= f)
            CallCC0 n t b -> CallCC0 n t (b >>>= f)
            Throw0 c e     -> Throw0 (c >>= f) (e >>= f)

$(deriveEq ''Exp0)
$(deriveShow ''Exp0)
$(deriveEq1 ''Exp0)
$(deriveShow1 ''Exp0)

lam0 :: Seq (Name, Type0) -> Exp0 Name -> Exp0 Name
lam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (`Seq.elemIndexR` ns)

callcc0 :: Name -> Type0 -> Exp0 Name -> Exp0 Name
callcc0 n t = CallCC0 n t . abstract1 n

-- Stmt0

data Stmt0 a =
    Decl0 a Type0
  | Defn0 a (Exp0 a)
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
