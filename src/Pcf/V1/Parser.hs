module Pcf.V1.Parser (readExp, readStmt, readTy) where

import           Bound               (abstract1)
import           Bound.Name          (Name (..))
import           Control.Applicative (Alternative (..))
import           Control.Monad       (guard, mzero)
import           Data.Foldable       (toList)
import           Data.List           (foldl')
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Pcf.Core.SExp       (SExp (..))
import           Pcf.V1.Types        (Exp (..), Stmt (..), Ty (..))

readTy :: Alternative m => SExp i Text -> m Ty
readTy (SAtom _ t) = if t == "Nat" then pure Nat else empty
readTy (SList _ ts) = case toList ts of
    [SAtom _ "->", l, r] -> Arr <$> readTy l <*> readTy r
    _                    -> empty

keywords :: Set Text
keywords = S.fromList ["ifz", "lam", "fix", "suc", "zero", "Nat", "->"]

-- TODO use applicative do and remove monad constraint
readExp :: (Monad m, Alternative m) => SExp i Text -> m (Exp Text)
readExp (SAtom _ t) = pure (if t == "zero" then Zero else Var t)
readExp (SList _ ts) = go ts where
    go ts = case toList ts of
        [SAtom _ "suc", y] -> Suc <$> readExp y
        [SAtom _ "ifz", g, t, e] -> Ifz <$> readExp g <*> readExp t <*> readExp e
        [SAtom _ "lam", SAtom _ n, ty, e] -> do
            guard (not (S.member n keywords))
            ty' <- readTy ty
            s <- abstract1 n <$> readExp e
            pure (Lam (Name n ()) ty' s)
        [SAtom _ "fix", SAtom _ n, ty, e] -> do
            guard (not (S.member n keywords))
            ty' <- readTy ty
            e' <- readExp e
            let s = abstract1 n e'
            pure (Fix (Name n ()) ty' s)
        l:r:rs -> assoc rs (App <$> readExp l <*> readExp r)
        _ -> empty
    assoc rs me = foldl' (\me r -> App <$> me <*> readExp r) me rs

readStmt :: (Monad m, Alternative m) => SExp i Text -> m (Stmt Text)
readStmt (SAtom _ t) = empty
readStmt (SList _ ts) = go ts where
    go ts = case toList ts of
        [SAtom _ "decl", SAtom _ n, ty] -> Decl n <$> readTy ty
        [SAtom _ "defn", SAtom _ n, e]  -> Defn n <$> readExp e
        _                               -> empty
