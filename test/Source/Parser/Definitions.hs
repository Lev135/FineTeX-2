{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Source.Parser.Definitions where
import Data.String (IsString(..))
import Language.FineTeX.Source.Parser.Definitions
import Language.FineTeX.Source.Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Utils

x, y :: Exp ()
[x, y] = EIdent <$> ["x", "y"]
f, g :: Exp () -> Exp ()
[f, g] = EApp . EIdent <$> ["f", "g"]
h, (+>) :: Exp () -> Exp () -> Exp ()
h = fmap EApp . EApp . EIdent $ "h"
(+>) = flip EBinOp "+"
infixl 5 +>
ev :: Exp () -> Exp ()
ev = EPrefOp "!"
evm :: Exp () -> Exp () -> Exp ()
evm = EApp . EPrefOp "!!"

instance IsString (Exp ()) where
  fromString = EStringLit . fromString

spec :: Spec
spec = do
  context "pExp" do
    it "string litteral" $
      prsI pExp "'abc'" `parses` "abc" `leaving` ""
    it "identifier" $
      prsI pExp "x" `parses` x `leaving` ""
    it "simple app" $
      prsI pExp "f x" `parses` f x `leaving` ""
    it "nested app" $
      prsI pExp "f (g x)" `parses` f (g x) `leaving` ""
    it "two args" $
      prsI pExp "h x y" `parses` h x y `leaving` ""
    it "simple +" $
      prsI pExp "x + y" `parses` x +> y `leaving` ""
    it "many +" $
      prsI pExp "x + y + 'a'" `parses` x +> y +> "a" `leaving` ""
    it "complex" $
      prsI pExp "f (x + !y) + !! (f x) x"
        `parses` f (x +> ev y) +> evm (f x) x `leaving` ""
    it "fails: empty" $
      prs' pExp "" `failsLeaving` ""
  context "process" do
    let st = ProcStatement
    it "empty" $
      prsI pProcess "" `failsLeaving` ""
    it "one" $
      prsI pProcess "@Foo x + y"
        `parses` st "Foo" (Just $ x +> y)
        `leaving` ""
    it "two" $
      prsI pProcess "@Foo x + y @Bar x"
        `parses` st "Foo" (Just $ x +> y)
        `leaving` "@Bar x"
    it "with next line" $
      prsI pProcess "@Foo x\nabc"
        `parses` st "Foo" (Just x)
        `leaving` "\nabc"
    it "without arguments" $
      prsI pProcess "@Foo @Bar x"
        `parses` st "Foo" Nothing
        `leaving` "@Bar x"
