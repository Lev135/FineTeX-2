module Language.FineTeX.Source where

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (MonadState)
import Data.Text (Text)
import Language.FineTeX.Source.Errors
import Language.FineTeX.Source.Parser.Definitions (pDefinitions)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Source.Preprocess
import Language.FineTeX.Source.Syntax
import Language.FineTeX.Utils
import Text.Megaparsec (runParser)

parseText :: (MonadState Definitions m, MonadError [SourceError] m) =>
  SourceId -> Text -> m Document
parseText srcId file = do
  let env = ParserEnv{ parserSource = srcId }
  defBlock <- case runParser (runScT pDefinitions `runReaderT` env) "" file of
    Left  e        -> throwError [ParseError e]
    Right defBlock -> pure defBlock
  preprocDefs defBlock
  pure Document {definitions = defBlock, body = []}
