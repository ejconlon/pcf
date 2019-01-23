module Pcf.V3.Parser where

import           Control.Applicative (Alternative (..))
import           Data.Foldable       (toList)
import qualified Data.Sequence       as Seq
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import           Pcf.Core.SExp       (SExp (..))
import           Pcf.V3.Types

readAtom :: Alternative m => (x -> m a) -> (SExp i x -> m a)
readAtom p (SAtom _ t) = p t
readAtom _ _ = empty

readPair :: Alternative m => (SExp i x -> m a) -> (SExp i x -> m b) -> (SExp i x -> m (a, b))
readPair pa pb (SList _ ts) =
    case toList ts of
        [ta, tb] -> (,) <$> pa ta <*> pb tb
        _        -> empty
readPair _ _ _ = empty

keywords :: Set Text
keywords = S.fromList
    [ "let", "case", "lam", "throw", "callcc", "data", "decl", "defn"
    , "Cont", "Fun", "the", "var", "wild", "con", "new", "_"
    ]

readName :: Alternative m => Text -> m Name
readName n = if S.member n keywords then empty else pure (Name n)

readIdent :: Alternative m => Text -> m Ident
readIdent n = if n == "_" then pure WildIdent else ConcreteIdent <$> readName n

readNameAtom :: Alternative m => SExp i Text -> m Name
readNameAtom = readAtom readName

readIdentAtom :: Alternative m => SExp i Text -> m Ident
readIdentAtom = readAtom readIdent

readTyX :: Alternative m => SExp i Text -> m TypeX
readTyX (SAtom _ t) = TyVarX <$> readName t
readTyX (SList _ ts) = case toList ts of
    [SAtom _ "Fun", SList _ us, r] -> TyFunX <$> traverse readTyX us <*> readTyX r
    [SAtom _ "Cont", t] -> TyContX <$> readTyX t
    _ -> empty

readIX :: Alternative m => SExp i Text -> m (Ident, TypeX)
readIX = readPair readIdentAtom readTyX

readPatL :: Alternative m => SExp i Text -> m PatL
readPatL = undefined

readPatX :: Alternative m => SExp i Text -> m PatX
readPatX s = uncurry PatX <$> readPair readPatL readExpX s

readExpX :: Alternative m => SExp i Text -> m ExpX
readExpX (SAtom _ t) = VarX <$> readName t
readExpX (SList _ ts) =
    case toList ts of
        [SAtom _ "let", n, e, u] -> LetX <$> readIdentAtom n <*> readExpX e <*> readExpX u
        [SAtom _ "the", e, t] -> TheX <$> readExpX e <*> readTyX t
        [SAtom _ "case", e, SList _ ps] -> CaseX <$> readExpX e <*> traverse readPatX ps
        [SAtom _ "throw", c, e] -> ThrowX <$> readExpX c <*> readExpX e
        [SAtom _ "lam", SList _ nts, e] -> LamX <$> traverse readIX nts <*> readExpX e
        [SAtom _ "callcc", n, t, e] -> CallCCX <$> readNameAtom n <*> readTyX t <*> readExpX e
        l:rs -> CallX <$> readExpX l <*> (Seq.fromList <$> traverse readExpX rs)
        _ -> empty

readConDefX :: Alternative m => SExp i Text -> m ConDefX
readConDefX (SAtom _ t) = (\n -> ConDef n Seq.empty) <$> readName t
readConDefX (SList _ ts) =
    case toList ts of
        n:us -> ConDef <$> readNameAtom n <*> (Seq.fromList <$> traverse readTyX us)
        _ -> empty

readStmtX :: Alternative m => SExp i Text -> m StmtX
readStmtX (SAtom _ t) = empty
readStmtX (SList _ ts) =
    case toList ts of
        [SAtom _ "decl", n, ty] -> Decl <$> readNameAtom n <*> readTyX ty
        [SAtom _ "defn", n, e]  -> Defn <$> readNameAtom n <*> readExpX e
        (SAtom _ "data"):n:mcs  -> Data <$> readNameAtom n <*> cs where
            cs = case mcs of
                [] -> pure (Seq.Empty)
                [SList _ cs] -> traverse readConDefX cs
                _ -> empty
        _                       -> empty
