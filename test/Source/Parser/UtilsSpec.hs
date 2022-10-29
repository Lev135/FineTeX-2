module Source.Parser.UtilsSpec where

import Language.FineTeX.Source.Parser.Utils
import Test.Hspec
import Test.Hspec.Megaparsec
import Utils

spec :: Spec
spec = do
  context "keyword" do
    let a = keyword "a"
    it "simple" $
      prs' a "a" `parses` "a" `leaving` ""
    it "with spaces" $
      prs' a "a  " `parses` "a" `leaving` ""
    it "letters with spaces" $
      prs' a "a b" `parses` "a" `leaving` "b"
    it "fail: letters" $
      prs' a "ab" `failsLeaving` "b"
    it "fail: numbers" $
      prs' a "a1" `failsLeaving` "1"
  context "ident" do
    it "simple" $
      prsi ident "abc" `parses` "abc" `leaving` ""
    it "with spaces" $
      prsi ident "abc  " `parses` "abc" `leaving` ""
    it "with numbers" $
      prsi ident "abc123 " `parses` "abc123" `leaving` ""
    it "nonAlphanum symbols" $
      prsi ident "a2c(" `parses` "a2c" `leaving` "("
    it "alphanum in the next word" $
      prsi ident "a2c b3d" `parses` "a2c" `leaving` "b3d"
    it "fails: empty" $
      prsi ident "" `failsLeaving` ""
    it "fails: num first" $
      prsi ident "123abc" `failsLeaving` "123abc"
  context "word" do
    it "simple" $
      prsi word "abc" `parses` "abc" `leaving` ""
    it "unicode symbols" $
      prsi word "α∇♠⁽" `parses` "α∇♠⁽" `leaving` ""
    it "with spaces" $
      prsi word "abc  " `parses` "abc" `leaving` ""
    it "with the next word" $
      prsi word "a2c b3d" `parses` "a2c" `leaving` "b3d"
    it "fails: empty" $
      prsi word "" `failsLeaving` ""
