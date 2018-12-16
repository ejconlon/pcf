module Pcf.V1.Parser (readExp, readStmt, readTy, readSExp, sexpParser) where

import           Bound                      (abstract1)
import           Bound.Name                 (Name (..))
import           Control.Applicative        (Alternative (..))
import           Control.Monad              (guard, mzero)
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Pcf.V1.Types               (Exp (..), SExp (..), Stmt (..), Ty (..))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

type Parser = MP.Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = MPCL.space MPC.space1 lineCmnt blockCmnt
  where
    lineCmnt  = MPCL.skipLineComment ";"
    blockCmnt = MPCL.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = MPCL.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = MPCL.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && c /= ' ' && c /= '\t' && c /= '\n'

atomParser :: Parser Text
atomParser = lexeme (MP.try (MP.takeWhile1P Nothing nonDelimPred))

satomParser :: Parser (SExp Text)
satomParser = SAtom <$> atomParser

slistParser :: Parser (SExp Text)
slistParser = SList <$> parens (MP.many (spaceConsumer *> sexpParser))

sexpParser :: Parser (SExp Text)
sexpParser = satomParser <|> slistParser

readSExp :: Text -> Maybe (SExp Text)
readSExp = MP.parseMaybe sexpParser

readTy :: Alternative m => SExp Text -> m Ty
readTy (SAtom t) = if t == "Nat" then pure Nat else empty
readTy (SList ts) = case ts of
    [SAtom "->", l, r] -> Arr <$> readTy l <*> readTy r
    _                  -> empty

keywords :: Set Text
keywords = Set.fromList ["ifz", "lam", "fix", "suc", "zero", "Nat", "->"]

-- TODO use applicative do and remove monad constraint
readExp :: (Monad m, Alternative m) => SExp Text -> m (Exp Text)
readExp (SAtom t) = pure (if t == "zero" then Zero else Var t)
readExp (SList ts) = go ts where
    go ts = case ts of
        [SAtom "suc", y] -> Suc <$> readExp y
        [SAtom "ifz", g, t, e] -> Ifz <$> readExp g <*> readExp t <*> readExp e
        [SAtom "lam", SAtom n, ty, e] -> do
            guard (not (Set.member n keywords))
            ty' <- readTy ty
            s <- abstract1 n <$> readExp e
            pure (Lam (Name n ()) ty' s)
        [SAtom "fix", SAtom n, ty, e] -> do
            guard (not (Set.member n keywords))
            ty' <- readTy ty
            e' <- readExp e
            let s = abstract1 n e'
            pure (Fix (Name n ()) ty' s)
        l:r:rs -> assoc rs (App <$> readExp l <*> readExp r)
        _ -> empty
    assoc rs me = foldl' (\me r -> App <$> me <*> readExp r) me rs

readStmt :: (Monad m, Alternative m) => SExp Text -> m (Stmt Text)
readStmt (SAtom t) = empty
readStmt (SList ts) = go ts where
    go ts = case ts of
        [SAtom "decl", SAtom n, ty] -> Decl n <$> readTy ty
        [SAtom "defn", SAtom n, e]  -> Defn n <$> readExp e
        _                           -> empty
