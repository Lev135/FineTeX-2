module Main (main) where

import qualified Source.Parser.UtilsSpec
import Test.Hspec

main :: IO ()
main = hspec do
  Source.Parser.UtilsSpec.spec
