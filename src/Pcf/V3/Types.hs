{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.Types where

import           Bound         (Scope, abstract, abstract1, (>>>=))
import           Control.Monad (ap)
import           Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.String   (IsString)
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

newtype Name = Name { unName :: Text } deriving (Generic, Eq, Ord, Show, IsString)

data Ident = ConcreteIdent Name | WildIdent deriving (Generic, Eq, Show)

data ConDef t = ConDef
    { conDefName :: Name
    , conDefTypes :: Seq t
    } deriving (Generic, Eq, Show)

data Stmt t e =
      Decl Name t
    | Defn Name e
    | Data Name (Seq (ConDef t))
    deriving (Generic, Eq, Show)

-- Defs

data DataDefs t = DataDefs
    { tyNameToConNames :: Map Name (Seq Name)
    , conNameToTyNameAndDef :: Map Name (Name, ConDef t)
    } deriving (Generic, Eq, Show)

declaredDataType :: Name -> DataDefs t -> Bool
declaredDataType n dds = M.member n (tyNameToConNames dds)

addDataDef :: Name -> Seq (ConDef t) -> DataDefs t -> DataDefs t
addDataDef n cds (DataDefs x y) = DataDefs x' y' where
    x' = M.insert n (conDefName <$> cds) x
    y' = foldl (\z cd -> M.insert (conDefName cd) (n, cd) z) y cds

emptyDataDefs :: DataDefs t
emptyDataDefs = DataDefs M.empty M.empty

-- ExpX and friends

data TypeX =
      TyVarX Name
    | TyFunX (Seq TypeX) TypeX
    | TyCallX TypeX (Seq TypeX)
    deriving (Generic, Eq, Show)

data PatL =
      VarPatL Ident
    | ConPatL Name (Seq Ident)
    deriving (Generic, Eq, Show)

data PatX = PatX PatL ExpX deriving (Generic, Eq, Show)

data ExpX =
      VarX Name
    | LetX Ident ExpX ExpX
    | CaseX ExpX (Seq PatX)
    | CallX ExpX (Seq ExpX)
    | LamX (Seq (Ident, TypeX)) ExpX
    | CallCCX Name TypeX ExpX
    | ThrowX ExpX ExpX
    | TheX ExpX TypeX
    deriving (Generic, Eq, Show)

type ConDefX = ConDef TypeX
type StmtX = Stmt TypeX ExpX
type DataDefsX = DataDefs TypeX

-- Exp0 and friends

data Type0 =
    TyCon0 Name
  | TyFun0 (Seq Type0) Type0
  | TyCont0 Type0
  deriving (Generic, Eq, Show)

data Pat0 a =
      VarPat0 Name (Scope () Exp0 a)
    | ConPat0 Name (Seq Name) (Scope Int Exp0 a)
    | WildPat0 (Exp0 a)
    deriving (Generic, Functor, Foldable, Traversable)

data Exp0 a =
      Var0 a
    | Let0 Name (Exp0 a) (Scope () Exp0 a)
    | Con0 Name (Seq (Exp0 a))
    | Case0 (Exp0 a) (Seq (Pat0 a))
    | Call0 (Exp0 a) (Seq (Exp0 a))
    | Lam0 (Seq (Name, Type0)) (Scope Int Exp0 a)
    | CallCC0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    | The0 (Exp0 a) Type0
    deriving (Generic, Functor, Foldable, Traversable)

data Dir0 =
      DirLetArg0
    | DirLetBody0
    | DirCallFun0
    | DirCallArg0 Int
    | DirLamBody0
    | DirConArg0 Int
    | DirCaseArg0
    | DirCasePat0 Int
    | DirCallCCBody0
    | DirThrowFun0
    | DirThrowArg0
    | DirThe0
    deriving (Eq, Show)

type Path0 = Seq Dir0

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    m >>= f =
        case m of
            Var0 a        -> f a
            Let0 n e b    -> Let0 n (e >>= f) (b >>>= f)
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

-- lam0 :: Seq (Name, Type0) -> Exp0 Name -> Exp0 Name
-- lam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (`Seq.elemIndexR` ns)

-- callcc0 :: Name -> Type0 -> Exp0 Name -> Exp0 Name
-- callcc0 n t = CallCC0 n t . abstract1 n

-- let0 :: Name -> Exp0 Name -> Exp0 Name -> Exp0 Name
-- let0 n e = Let0 n e . abstract1 n

-- varPat0 :: Name -> Exp0 Name -> Pat0 Name
-- varPat0 n = VarPat0 n . abstract1 n

-- conPat0 :: Name -> Seq Name -> Exp0 Name -> Pat0 Name
-- conPat0 n ns = ConPat0 n ns . abstract (`Seq.elemIndexR` ns)

type ConDef0 = ConDef Type0
type Stmt0 = Stmt Type0 (Exp0 Name)
type DataDefs0 = DataDefs Type0
