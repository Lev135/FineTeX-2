module Language.FineTeX.Source.Parser.Utils (
  ParserEnv(..), ParserM, BaseParserM,
  L.ScT, runScT,
  located,
  L.nonIndented,
  headedMany, headedSome,
  lineFold, betweenSymbols,
  L.symbol, keyword, ident, word, stringLit
) where

import Control.Monad.RWS (MonadReader, asks)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.FineTeX.Source.Syntax (PText)
import Language.FineTeX.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer.Stateful (MonadParsecSc)
import qualified Text.Megaparsec.Char.Lexer.Stateful as L

newtype ParserEnv
  = ParserEnv { parserSource :: SourceId }

type ParserM m = (MonadReader ParserEnv m, MonadParsecSc Void Text m)
type BaseParserM m = MonadParsec Void Text m

-- | Run `ScT`, used by FineTeX's parser
runScT :: MonadParsec Void Text m => L.ScT m a -> m a
runScT = (`L.runScT` sc)


-- | Save element's location into `Info`
located :: ParserM m => m a -> m (Posed a)
located pa = do
  b <- getOffset
  a <- pa
  e <- getOffset
  locSource <- asks parserSource
  pure $ Posed (Just $ Pos locSource (b, e)) a

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
ident :: ParserM m => m PText
ident = L.lexeme $ located do
  a <- satisfy isAlpha
  as <- takeWhileP Nothing isAlphaNum
  pure $ T.cons a as

-- | Parse a nonempty sequence of non-space characters
word :: ParserM m => m PText
word = L.lexeme $ located $ takeWhile1P Nothing (not . isSpace)

-- | Parse string literal: sequence of arbitrary characters enclosed in quotes
stringLit :: ParserM m => m PText
stringLit = L.lexeme . located $ choice $
  flip map ['\'', '"'] \c ->
    char c *> takeWhileP Nothing (`notElem` ['\r', '\n' , c]) <* char c
