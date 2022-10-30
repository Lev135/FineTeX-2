{-# OPTIONS_GHC -Wno-orphans #-}
module Utils where
import Control.Monad (void)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Source.Syntax (Posed(..))
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

instance (IsString a) => IsString (Posed a) where
  fromString = Posed Nothing . fromString

type Parser = ScT (Parsec Void Text)

prs :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
prs p = runParser (runScT $ space *> p) ""

prs' :: Parser a -> Text ->
  (State Text Void, Either (ParseErrorBundle Text Void) a)
prs' p str = runParser' (runScT $ space *> p) (initialState str)

parses :: (State s e, Either (ParseErrorBundle s e) a) -> a ->
  ((State s e, Either (ParseErrorBundle s e) a), a)
parses = (,)
infixl 2 `parses`

leaving :: (ShowErrorComponent e, VisualStream s, TraversableStream s,
  Show a, Show s, Eq a, Eq s) =>
  ((State s e, Either (ParseErrorBundle s e) a), a) -> s -> IO ()
leaving = uncurry shouldParseLeaving
infixl 2 `leaving`

shouldParseLeaving ::
  (ShowErrorComponent e, VisualStream s, TraversableStream s,
    Show a, Show s, Eq a, Eq s) =>
  (State s e, Either (ParseErrorBundle s e) a) -> a -> s -> IO ()
shouldParseLeaving p a txt = do
  snd p `shouldParse` a
  p `succeedsLeaving` txt
