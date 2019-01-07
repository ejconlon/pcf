module Pcf.V2.Parser (readExp, readStmt, readTy) where

import           Control.Applicative        (Alternative (..))
import           Data.Foldable              (toList)
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Pcf.Core.SExp              (SExp (..))
import           Pcf.Core.Sub
import           Pcf.V2.Types               (Exp (..), ExpF (..), ExpN (..),
                                             Stmt (..), Ty (..))

readTy :: Alternative m => SExp i Text -> m Ty
readTy (SAtom _ t) = if t == "Nat" then pure Nat else empty
readTy (SList _ ts) = case toList ts of
    [SAtom _ "->", l, r] -> Arr <$> readTy l <*> readTy r
    _                    -> empty

keywords :: Set Text
keywords = Set.fromList ["ifz", "lam", "fix", "suc", "zero", "Nat", "->"]

-- What info do we want to extract? Do we annotate with the sum of annotations?
-- Do we get that by default with slist ann, for example?
readExp :: Alternative m => SExp i Text -> m (Exp Text Text)
readExp (SAtom _ t) = pure (if t == "zero" then wrapScope Zero else pure t)
readExp (SList _ ts) = go ts where
    go ts = case toList ts of
        [SAtom _ "suc", y] -> wrapScope . Suc <$> readExp y
        [SAtom _ "ifz", g, t, e] -> (\g' t' e' -> wrapScope (Ifz g' t' e')) <$> readExp g <*> readExp t <*> readExp e
        [SAtom _ "lam", SAtom _ n, ty, e] ->
            if Set.member n keywords
                then empty
                else (\ty' e' -> binderScope (abstract1 (ExpN (Name n ()) ty') n e')) <$> readTy ty <*> readExp e
        l:r:rs -> assoc rs ((\l' r' -> wrapScope (App l' r')) <$> readExp l <*> readExp r)
        _ -> empty
    assoc rs me = foldl' (\me' r -> (\l' r' -> wrapScope (App l' r')) <$> me' <*> readExp r) me rs

readStmt :: Alternative m => SExp i Text -> m (Stmt Text Text)
readStmt (SAtom _ t) = empty
readStmt (SList _ ts) = go ts where
    go ts = case toList ts of
        [SAtom _ "decl", SAtom _ n, ty] -> Decl n <$> readTy ty
        [SAtom _ "defn", SAtom _ n, e]  -> Defn n <$> readExp e
        _                               -> empty
