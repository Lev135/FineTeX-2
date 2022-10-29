{-# OPTIONS_GHC -Wno-orphans #-}
module Utils where
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Utils
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

i :: a -> Info i a
i = I undefined

instance (IsString a) => IsString (Info i a) where
  fromString = i . fromString

type Parser = ScT (Parsec Void Text)

prs :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
prs p = runParser (runScT $ space *> p) ""

prs' :: Parser a -> Text ->
  (State Text Void, Either (ParseErrorBundle Text Void) a)
prs' p str = runParser' (runScT $ space *> p) (initialState str)


parses :: (State s e, Either (ParseErrorBundle s e) a) -> a ->
  ((State s e, Either (ParseErrorBundle s e) a), a)
parses = (,)

leaving :: (ShowErrorComponent e, VisualStream s, TraversableStream s,
  Show a, Show s, Eq a, Eq s) =>
  ((State s e, Either (ParseErrorBundle s e) a), a) -> s -> IO ()
leaving = uncurry shouldParseLeaving

shouldParseLeaving ::
  (ShowErrorComponent e, VisualStream s, TraversableStream s,
    Show a, Show s, Eq a, Eq s) =>
  (State s e, Either (ParseErrorBundle s e) a) -> a -> s -> IO ()
shouldParseLeaving p a txt = do
  snd p `shouldParse` a
  p `succeedsLeaving` txt
