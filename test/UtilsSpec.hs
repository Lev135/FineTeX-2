{-# LANGUAGE TypeApplications #-}
module UtilsSpec where

import qualified Data.Map as M
import Language.FineTeX.Utils
import Test.Hspec

data Error
  = Error0
  | Error1 Pos
  | Error2 Pos Pos
  | ErrorMany [Pos]
  deriving (Eq, Show)

instance PrettyErr Error where
  prettyErr = \case
    Error0       -> [nopos "Error 0"]
    Error1 p     -> [pos p "Error 1"]
    Error2 p p'  -> [pos p "Error 2", pos p' "Error 2 linked"]
    ErrorMany ps -> nopos "ErrorMany" : map (`pos` "item") ps

srcs :: M.Map SourceId Src
srcs = M.fromList [(SourceId 0, Src "file0.txt" ["abcdef", "ABCDEF", "12345"])]

spec :: Spec
spec = do
  context "renderErrs" do
    let rnd = renderErrors @Error srcs
    it "no errors" $
      rnd [] `shouldBe` ""
    it "cut right bound" $
      rnd [Error0, Error1 (Pos (SourceId 0) (2, 99))]
        `shouldBe` "Error 0\nfile0.txt:1:3: Error 1\nabcdef\n  ^~~^"
