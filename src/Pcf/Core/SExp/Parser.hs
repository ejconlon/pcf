module Pcf.Core.SExp.Parser where

import           Control.Applicative        ((<|>))
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Pcf.Core.SExp              (SExp (..))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

data Anno = Anno { annoStart :: MP.SourcePos, annoEnd :: MP.SourcePos }
    deriving (Generic, Eq, Show)

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
slistParser af = around (\s e -> SList (af s e)) (Seq.fromList <$> parens (MP.many (spaceConsumer *> sexpParser af)))

sexpParser :: AnnoFunc i -> Parser (SExp i Text)
sexpParser af = satomParser af <|> slistParser af

readSExpWith :: AnnoFunc i -> Text -> Maybe (SExp i Text)
readSExpWith af = MP.parseMaybe (sexpParser af)

readSExpUnit :: Text -> Maybe (SExp () Text)
readSExpUnit = readSExpWith (\_ _ -> ())

readSExpAnno :: Text -> Maybe (SExp Anno Text)
readSExpAnno = readSExpWith Anno
