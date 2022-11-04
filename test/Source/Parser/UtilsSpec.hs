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
      prs' ident "abc" `parses'` "abc" `leaving` ""
    it "with spaces" $
      prs' ident "abc  " `parses'` "abc" `leaving` ""
    it "with numbers" $
      prs' ident "abc123 " `parses'` "abc123" `leaving` ""
    it "nonAlphanum symbols" $
      prs' ident "a2c(" `parses'` "a2c" `leaving` "("
    it "alphanum in the next word" $
      prs' ident "a2c b3d" `parses'` "a2c" `leaving` "b3d"
    it "fails: empty" $
      prs' ident "" `failsLeaving` ""
    it "fails: num first" $
      prs' ident "123abc" `failsLeaving` "123abc"
  context "word" do
    it "simple" $
      prs' word "abc" `parses'` "abc" `leaving` ""
    it "unicode symbols" $
      prs' word "α∇♠⁽" `parses'` "α∇♠⁽" `leaving` ""
    it "with spaces" $
      prs' word "abc  " `parses'` "abc" `leaving` ""
    it "with the next word" $
      prs' word "a2c b3d" `parses'` "a2c" `leaving` "b3d"
    it "fails: empty" $
      prs' word "" `failsLeaving` ""
  context "string literal" do
    it "simple '" $
      prs' stringLit "'abc'" `parses'` "abc" `leaving` ""
    it "simple \"" $
      prs' stringLit "\"abc\"" `parses'` "abc" `leaving` ""
    it "next without spaces" $
      prs' stringLit "'abc'd" `parses'` "abc" `leaving` "d"
    it "next with spaces" $
      prs' stringLit "'abc'  d" `parses'` "abc" `leaving` "d"
    it "fail: eof" $
      prs' stringLit "'abc" `failsLeaving` ""
    it "fail: eol" $
      prs' stringLit "'abc\nd'" `failsLeaving` "\nd'"
