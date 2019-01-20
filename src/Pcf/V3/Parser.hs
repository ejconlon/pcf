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
import           Pcf.Core.Sub
import           Pcf.V3.Types

readTy0 :: Alternative m => SExp i Text -> m Type0
readTy0 (SAtom _ t) = if t == "Bool" then pure TyBool0 else empty
readTy0 (SList _ ts) = case toList ts of
    [SAtom _ "->", SList _ us, r] -> TyArr0 <$> traverse readTy0 us <*> readTy0 r
    [SAtom _ "Cont", r]           -> TyCont0 <$> readTy0 r
    _                             -> empty

keywords :: Set Text
keywords = S.fromList ["if", "lam", "throw", "callcc", "True", "False", "Bool", "Cont", "->"]

readN :: Alternative m => Text -> m Text
readN n = if S.member n keywords then empty else pure n

readNT :: Alternative m => SExp i Text -> m (Text, Type0)
readNT (SList _ ts) =
    case toList ts of
        [SAtom _ n, t] -> (,) <$> readN n <*> readTy0 t
        _              -> empty
readNT _ = empty

readExp0 :: Alternative m => SExp i Text -> m (Exp0 Text)
readExp0 (SAtom _ t) =
    case t of
        "True"  -> pure (Bool0 True)
        "False" -> pure (Bool0 False)
        _       -> Var0 <$> readN t
readExp0 (SList _ ts) =
    case toList ts of
        [SAtom _ "if", g, t, e] -> If0 <$> readExp0 g <*> readExp0 t <*> readExp0 e
        [SAtom _ "throw", c, e] -> Throw0 <$> readExp0 c <*> readExp0 e
        [SAtom _ "lam", SList _ nts, e] -> lam0 <$> traverse readNT nts <*> readExp0 e
        [SAtom _ "callcc", SAtom _ n, t, e] -> callcc0 <$> readN n <*> readTy0 t <*> readExp0 e
        l:rs -> (\l' rs' -> Call0 l' (Seq.fromList rs')) <$> readExp0 l <*> traverse readExp0 rs
        _ -> empty

readStmt0 :: Alternative m => SExp i Text -> m (Stmt0 Text)
readStmt0 (SAtom _ t) = empty
readStmt0 (SList _ ts) =
    case toList ts of
        [SAtom _ "decl", SAtom _ n, ty] -> Decl0 n <$> readTy0 ty
        [SAtom _ "defn", SAtom _ n, e]  -> Defn0 n <$> readExp0 e
        _                               -> empty
