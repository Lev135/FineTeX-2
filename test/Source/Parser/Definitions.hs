{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Source.Parser.Definitions where
import Data.String (IsString(..))
import Language.FineTeX.Source.Parser.Definitions
import Language.FineTeX.Source.Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Utils

x, y :: Exp
[x, y] = EIdent <$> ["x", "y"]
f, g :: Exp -> Exp
[f, g] = EApp . EIdent <$> ["f", "g"]
h, (+>) :: Exp -> Exp -> Exp
h = fmap EApp . EApp . EIdent $ "h"
(+>) = flip EBinOp "+"
infixl 5 +>
ev :: Exp -> Exp
ev = EPrefOp "!"
evm :: Exp -> Exp -> Exp
evm = EApp . EPrefOp "!!"

instance IsString Exp where
  fromString = EStringLit . fromString

spec :: Spec
spec = do
  context "pExp" do
    it "string litteral" $
      prs' pExp "'abc'" `parses` "abc" `leaving` ""
    it "identifier" $
      prs' pExp "x" `parses` x `leaving` ""
    it "simple app" $
      prs' pExp "f x" `parses` f x `leaving` ""
    it "nested app" $
      prs' pExp "f (g x)" `parses` f (g x) `leaving` ""
    it "two args" $
      prs' pExp "h x y" `parses` h x y `leaving` ""
    it "simple +" $
      prs' pExp "x + y" `parses` x +> y `leaving` ""
    it "many +" $
      prs' pExp "x + y + 'a'" `parses` x +> y +> "a" `leaving` ""
    it "complex" $
      prs' pExp "f (x + !y) + !! (f x) x"
        `parses` f (x +> ev y) +> evm (f x) x `leaving` ""
    it "fails: empty" $
      prs' pExp "" `failsLeaving` ""
  context "process" do
    let st = ProcStatement
    it "empty" $
      prs' pProcess "" `failsLeaving` ""
    it "one" $
      prs' pProcess "@Foo x + y"
        `parses` st "Foo" (Just $ x +> y)
        `leaving` ""
    it "two" $
      prs' pProcess "@Foo x + y @Bar x"
        `parses` st "Foo" (Just $ x +> y)
        `leaving` "@Bar x"
    it "with next line" $
      prs' pProcess "@Foo x\nabc"
        `parses` st "Foo" (Just x)
        `leaving` "\nabc"
    it "without arguments" $
      prs' pProcess "@Foo @Bar x"
        `parses` st "Foo" Nothing
        `leaving` "@Bar x"
