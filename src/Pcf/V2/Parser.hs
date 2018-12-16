module Pcf.V2.Parser (readExp, readStmt, readTy, readSExp, sexpParser) where

import           Control.Applicative        (Alternative (..))
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Pcf.Core.Sub
import           Pcf.V2.Types               (Exp (..), ExpF (..), ExpN (..), SExp (..), Stmt (..), Ty (..))
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

readExp :: Alternative m => SExp Text -> m (Exp Text Text)
readExp (SAtom t) = pure (if t == "zero" then (wrapScope Zero) else (pure t))
readExp (SList ts) = go ts where
    go ts = case ts of
        [SAtom "suc", y] -> wrapScope . Suc <$> readExp y
        [SAtom "ifz", g, t, e] -> (\g' t' e' -> wrapScope (Ifz g' t' e')) <$> readExp g <*> readExp t <*> readExp e
        [SAtom "lam", SAtom n, ty, e] ->
            if Set.member n keywords
                then empty
                else (\ty' e' -> boundScope (abstract1 (ExpN (Name n ()) ty') n e')) <$> readTy ty <*> readExp e
        l:r:rs -> assoc rs ((\l' r' -> wrapScope (App l' r')) <$> readExp l <*> readExp r)
        _ -> empty
    assoc rs me = foldl' (\me' r -> (\l' r' -> wrapScope (App l' r')) <$> me' <*> readExp r) me rs

readStmt :: Alternative m => SExp Text -> m (Stmt Text Text)
readStmt (SAtom t) = empty
readStmt (SList ts) = go ts where
    go ts = case ts of
        [SAtom "decl", SAtom n, ty] -> Decl n <$> readTy ty
        [SAtom "defn", SAtom n, e]  -> Defn n <$> readExp e
        _                           -> empty
