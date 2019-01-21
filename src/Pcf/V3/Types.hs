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
      TyCon0 Name
    | TyFun0 (Seq Type0) Type0
    | TyCont0 Type0
    deriving (Generic, Eq, Show)

-- Exp0 and friends

data Pat0 a =
      VarPat0 Name (Scope () Exp0 a)
    | ConPat0 Name (Seq Name) (Scope Int Exp0 a)
    | WildPat0 (Exp0 a)
    deriving (Generic, Functor, Foldable, Traversable)

data Exp0 a =
      Var0 a
    | Let0 (Seq (Name, Exp0 a)) (Scope Int Exp0 a)
    | Con0 Name (Seq (Exp0 a))
    | Case0 (Exp0 a) (Seq (Pat0 a))
    | Call0 (Exp0 a) (Seq (Exp0 a))
    | Lam0 (Seq (Name, Type0)) (Scope Int Exp0 a)
    | CallCC0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    | The0 (Exp0 a) Type0
    deriving (Generic, Functor, Foldable, Traversable)

-- data Dir0 =
--       DirCallFun0
--     | DirCallArg0 Int
--     | DirLamBody0
--     | DirConArg0 Int
--     | DirCaseTarget0
--     | DirCasePat0 Int
--     | DirCallCCBody0
--     | DirThrowFun0
--     | DirThrowArg0
--     deriving (Eq, Show)

-- type Path0 = Seq Dir0

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    m >>= f =
        case m of
            Var0 a        -> f a
            Let0 nes b    -> Let0 ((\(n, e) -> (n, e >>= f)) <$> nes) (b >>>= f)
            Con0 n xs     -> Con0 n ((>>= f) <$> xs)
            Case0 e ps    -> Case0 (e >>= f) (flip patBind0 f <$> ps)
            Call0 e xs    -> Call0 (e >>= f) ((>>= f) <$> xs)
            Lam0 nts b    -> Lam0 nts (b >>>= f)
            CallCC0 n t b -> CallCC0 n t (b >>>= f)
            Throw0 c e    -> Throw0 (c >>= f) (e >>= f)
            The0 e t      -> The0 (e >>= f) t

patBind0 :: Pat0 a -> (a -> Exp0 b) -> Pat0 b
patBind0 p f =
    case p of
        VarPat0 n b -> VarPat0 n (b >>>= f)
        ConPat0 n ns b -> ConPat0 n ns (b >>>= f)
        WildPat0 e -> WildPat0 (e >>= f)

$(deriveEq ''Pat0)
$(deriveShow ''Pat0)
$(deriveEq1 ''Pat0)
$(deriveShow1 ''Pat0)

$(deriveEq ''Exp0)
$(deriveShow ''Exp0)
$(deriveEq1 ''Exp0)
$(deriveShow1 ''Exp0)

lam0 :: Seq (Name, Type0) -> Exp0 Name -> Exp0 Name
lam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (`Seq.elemIndexR` ns)

callcc0 :: Name -> Type0 -> Exp0 Name -> Exp0 Name
callcc0 n t = CallCC0 n t . abstract1 n

let0 :: Seq (Name, Exp0 Name) -> Exp0 Name -> Exp0 Name
let0 nes = let ns = fst <$> nes in Let0 nes . abstract (`Seq.elemIndexR` ns)

varPat0 :: Name -> Exp0 Name -> Pat0 Name
varPat0 n = VarPat0 n . abstract1 n

conPat0 :: Name -> Seq Name -> Exp0 Name -> Pat0 Name
conPat0 n ns = ConPat0 n ns . abstract (`Seq.elemIndexR` ns)

-- Stmt0 and friends

data ConDef0 = ConDef0 Name (Seq (Name, Type0)) deriving (Generic, Eq, Show)

conDefName :: ConDef0 -> Name
conDefName (ConDef0 n _) = n

data Stmt0 =
    Decl0 Name Type0
  | Defn0 Name (Exp0 Name)
  | Data0 Name (Seq ConDef0)
  deriving (Generic, Eq, Show)
