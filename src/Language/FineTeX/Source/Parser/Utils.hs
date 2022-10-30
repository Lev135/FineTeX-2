module Language.FineTeX.Source.Parser.Utils (
  ParserM, BaseParserM,
  L.ScT, runScT,
  PosOff, located,
  L.nonIndented,
  headedMany, headedSome,
  lineFold, betweenSymbols,
  L.symbol, keyword, ident, word, stringLit
) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.FineTeX.Utils (Info(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer.Stateful (MonadParsecSc)
import qualified Text.Megaparsec.Char.Lexer.Stateful as L

type ParserM m = (MonadParsecSc Void Text m)
type BaseParserM m = MonadParsec Void Text m

-- | Run `ScT`, used by FineTeX's parser
runScT :: MonadParsec Void Text m => L.ScT m a -> m a
runScT = (`L.runScT` sc)

-- | Range of source offsets, where element is situated
type PosOff = (Int, Int)

-- | Save element's location into `Info`
located :: ParserM m => m a -> m (Info PosOff a)
located pa = do
  b <- getOffset
  a <- pa
  e <- getOffset
  pure $ I (b, e) a

-- | Skip comment (without @eol@ after it)
skipLineComment :: BaseParserM m => m ()
skipLineComment = L.skipLineComment "//"

-- | Skip spaces and comments
sc, scn :: BaseParserM m => m ()
sc = L.space hspace1 skipLineComment empty
scn = L.space space1 skipLineComment empty


headedMany, headedSome :: ParserM m => m ([el] -> a) -> m el -> m a
headedMany = L.headedMany scn scn
headedSome = L.headedSome scn scn
infixr 1 `headedMany`
infixr 1 `headedSome`

lineFold :: ParserM m => m a -> m a
lineFold = L.lineFold scn

-- | Parse something between given symbols
betweenSymbols :: ParserM m => Text -> Text -> m a -> m a
betweenSymbols b e = between (L.symbol b) (L.symbol e)

-- | Parse a keyword lexeme and ensure, that there is no alphaNum character
-- after it
keyword :: ParserM m => Text -> m Text
keyword w = L.lexeme $ string w <* notFollowedBy alphaNumChar

-- | Parse an identifier: alpha character, followed by alphaNum characters
ident :: ParserM m => m (Info PosOff Text)
ident = L.lexeme $ located do
  a <- satisfy isAlpha
  as <- takeWhileP Nothing isAlphaNum
  pure $ T.cons a as

-- | Parse a nonempty sequence of non-space characters
word :: ParserM m => m (Info PosOff Text)
word = L.lexeme $ located $ takeWhile1P Nothing (not . isSpace)

-- | Parse string literal: sequence of arbitrary characters enclosed in quotes
stringLit :: ParserM m => m (Info PosOff Text)
stringLit = L.lexeme . located $ choice $
  flip map ['\'', '"'] \c ->
    char c *> takeWhileP Nothing (`notElem` ['\r', '\n' , c]) <* char c
