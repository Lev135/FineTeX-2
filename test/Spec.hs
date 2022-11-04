module Main (main) where

import qualified Source.Parser.DefinitionsSpec
import qualified Source.Parser.UtilsSpec
import Test.Hspec
import qualified UtilsSpec

main :: IO ()
main = hspec do
  Source.Parser.UtilsSpec.spec
  Source.Parser.DefinitionsSpec.spec
  UtilsSpec.spec
