module Pcf.V2.Parser (Anno, readExp, readStmt, readTy, readSExpAnno, readSExpUnit, readSExpWith, sexpParser) where

import           Control.Applicative        (Alternative (..))
import           Data.Foldable              (toList)
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Pcf.Core.Sub
import           Pcf.V2.Info
import           Pcf.V2.Types               (Exp (..), ExpF (..), ExpN (..), SExp (..),
                                             Stmt (..), Ty (..))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

type Parser = MP.Parsec Void Text

type AnnoFunc i = MP.SourcePos -> MP.SourcePos -> i

around :: (MP.SourcePos -> MP.SourcePos -> a -> b) -> Parser a -> Parser b
around f pa = (\s a e -> f s e a) <$> MP.getSourcePos <*> pa <*> MP.getSourcePos

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

satomParser :: AnnoFunc i -> Parser (SExp i Text)
satomParser af = around (\s e -> SAtom (af s e)) atomParser

slistParser :: AnnoFunc i -> Parser (SExp i Text)
slistParser af = around (\s e -> SList (af s e)) (V.fromList <$> parens (MP.many (spaceConsumer *> sexpParser af)))

sexpParser :: AnnoFunc i -> Parser (SExp i Text)
sexpParser af = satomParser af <|> slistParser af

readSExpWith :: AnnoFunc i -> Text -> Maybe (SExp i Text)
readSExpWith af = MP.parseMaybe (sexpParser af)

readSExpUnit :: Text -> Maybe (SExp () Text)
readSExpUnit = readSExpWith (\_ _ -> ())

readSExpAnno :: Text -> Maybe (SExp Anno Text)
readSExpAnno = readSExpWith Anno

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
                else (\ty' e' -> boundScope (abstract1 (ExpN (Name n ()) ty') n e')) <$> readTy ty <*> readExp e
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
