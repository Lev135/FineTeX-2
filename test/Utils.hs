{-# LANGUAGE TypeApplications #-}
module Utils where

import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Data (Data(..), Typeable, cast)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Utils
import qualified Language.FineTeX.Utils as Fine
import Test.Hspec (Expectation)
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = ReaderT ParserEnv (ScT (Parsec Void Text))

trivEnv :: ParserEnv
trivEnv = ParserEnv $ SourceId 0

prs :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
prs p = runParser (runScT $ (space *> p) `runReaderT` trivEnv) ""

prs' :: Parser a -> Text ->
  (State Text Void, Either (ParseErrorBundle Text Void) a)
prs' p str = runParser' (runScT $ (space *> p) `runReaderT` trivEnv) (initialState str)

parses :: (State s e, Either (ParseErrorBundle s e) a) -> a ->
  ((State s e, Either (ParseErrorBundle s e) a), a)
parses = (,)
infixl 2 `parses`

leaving :: (HasCallStack, ShowErrorComponent e, VisualStream s, TraversableStream s,
  Show a, Show s, Eq a, Eq s) =>
  ((State s e, Either (ParseErrorBundle s e) a), a) -> s -> Expectation
leaving = uncurry shouldParseLeaving
infixl 2 `leaving`

shouldParseLeaving ::
  (HasCallStack, ShowErrorComponent e, VisualStream s, TraversableStream s,
    Show a, Show s, Eq a, Eq s) =>
  (State s e, Either (ParseErrorBundle s e) a) -> a -> s -> Expectation
shouldParseLeaving p a txt = do
  snd p `shouldParse` a
  p `succeedsLeaving` txt

castTo :: forall b a. (Typeable a, Typeable b) => a -> Maybe b
castTo = cast

removePos :: Data a => a -> a
removePos = gmapT $ \x ->
  case castTo @(Maybe Fine.Pos) x of
    Nothing -> removePos x
    Just _  -> fromJust $ cast @(Maybe Fine.Pos) Nothing

parses' :: (Ord (Token s), Ord e, Data e, Data s, Data (Token s), Data a) =>
  (State s e, Either (ParseErrorBundle s e) a) -> a ->
  ((State s e, Either (ParseErrorBundle s e) a), a)
parses' = fmap removePos . parses
infixl 2 `parses'`
