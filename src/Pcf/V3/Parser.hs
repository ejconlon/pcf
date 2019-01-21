module Pcf.V3.Parser where

import           Control.Applicative (Alternative (..))
import           Data.Foldable       (toList)
import           Data.List           (foldl')
import qualified Data.Sequence       as Seq
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Vector         as V
import           Pcf.Core.SExp       (SExp (..))
import           Pcf.V3.Types

readTy0 :: Alternative m => SExp i Text -> m Type0
readTy0 (SAtom _ t) = pure (TyCon0 t)
readTy0 (SList _ ts) = case toList ts of
    [SAtom _ "Fun", SList _ us, r] -> TyFun0 <$> traverse readTy0 us <*> readTy0 r
    [SAtom _ "Cont", r]            -> TyCont0 <$> readTy0 r
    _                              -> empty

keywords :: Set Text
keywords = S.fromList
    [ "let", "case", "lam", "throw", "callcc", "data", "decl", "defn"
    , "Cont", "Fun", "the", "var", "wild", "con"
    ]

readN :: Alternative m => Text -> m Text
readN n = if S.member n keywords then empty else pure n

readAtom :: Alternative m => (x -> m a) -> (SExp i x -> m a)
readAtom p (SAtom _ t) = p t
readAtom _ _ = empty

readPair :: Alternative m => (SExp i x -> m a) -> (SExp i x -> m b) -> (SExp i x -> m (a, b))
readPair pa pb (SList _ ts) =
    case toList ts of
        [ta, tb] -> (,) <$> pa ta <*> pb tb
        _        -> empty
readPair _ _ _ = empty

readNT :: Alternative m => SExp i Text -> m (Text, Type0)
readNT = readPair (readAtom readN) readTy0

readNE :: Alternative m => SExp i Text -> m (Text, Exp0 Text)
readNE = readPair (readAtom readN) readExp0

readPat0 :: Alternative m => SExp i Text -> m (Pat0 Text)
readPat0 (SList _ ts) =
    case toList ts of
        [SAtom _ "var", SAtom _ n, e] -> varPat0 <$> readN n <*> readExp0 e
        [SAtom _ "wild", e] -> WildPat0 <$> readExp0 e
        [SAtom _ "con", SAtom _ n, SList _ ns, e] -> conPat0 <$> readN n <*> traverse (readAtom readN) ns <*> readExp0 e
readPat0 _ = empty

readExp0 :: Alternative m => SExp i Text -> m (Exp0 Text)
readExp0 (SAtom _ t) = Var0 <$> readN t
readExp0 (SList _ ts) =
    case toList ts of
        [SAtom _ "let", (SList _ nes), e] -> let0 <$> traverse readNE nes <*> readExp0 e
        [SAtom _ "the", e, t] -> The0 <$> readExp0 e <*> readTy0 t
        (SAtom _ "case"):e:ps -> Case0 <$> readExp0 e <*> (Seq.fromList <$> traverse readPat0 ps)
        [SAtom _ "throw", c, e] -> Throw0 <$> readExp0 c <*> readExp0 e
        [SAtom _ "lam", SList _ nts, e] -> lam0 <$> traverse readNT nts <*> readExp0 e
        [SAtom _ "callcc", SAtom _ n, t, e] -> callcc0 <$> readN n <*> readTy0 t <*> readExp0 e
        l:rs -> (\l' rs' -> Call0 l' (Seq.fromList rs')) <$> readExp0 l <*> traverse readExp0 rs
        _ -> empty

readConDef0 :: Alternative m => SExp i Text -> m ConDef0
readConDef0 (SAtom _ t) = pure (ConDef0 t Seq.empty)
readConDef0 (SList _ ts) =
    case toList ts of
        (SAtom _ n):us -> ConDef0 n <$> (Seq.fromList <$> traverse readNT us)
        _              -> empty

readStmt0 :: Alternative m => SExp i Text -> m Stmt0
readStmt0 (SAtom _ t) = empty
readStmt0 (SList _ ts) =
    case toList ts of
        [SAtom _ "decl", SAtom _ n, ty]         -> Decl0 n <$> readTy0 ty
        [SAtom _ "defn", SAtom _ n, e]          -> Defn0 n <$> readExp0 e
        [SAtom _ "data", SAtom _ n, SList _ cs] -> Data0 n <$> traverse readConDef0 cs
        _                                       -> empty
