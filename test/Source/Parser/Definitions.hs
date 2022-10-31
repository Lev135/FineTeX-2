{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
module Source.Parser.Definitions where
import Data.String (IsString(..))
import Language.FineTeX.Source.Parser.Definitions
import Language.FineTeX.Source.Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.RawString.QQ
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
instance IsString RegExp where
  fromString = REString . fromString
instance IsString a => IsString (Maybe a) where
  fromString "" = Nothing
  fromString s  = Just $ fromString s

spec :: Spec
spec = do
  let st = ProcStatement
      pmel = PatMatchEl

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
  context "patMatchExp" do
    let p = prs' (many pPatMatchEl)
    it "empty" $
      p "" `parses` [] `leaving` ""
    it "one with var" $
      p "(a : Word)"
        `parses` [pmel (Just "a") REWord]
        `leaving` ""
    it "one without var" $
      p "Word"
        `parses` [pmel Nothing REWord]
        `leaving` ""
    it "two" $
      p "(a : Word) Word"
        `parses` [pmel (Just "a") REWord, pmel Nothing REWord]
        `leaving` ""
  context "env definition" do
    let p = prs' pDefEnv
        d = DefEnv
    it "empty definition" $
      p "env => " `parses` d "env" [] Nothing [] `leaving` ""
    it "with proc statement" $
      p "env => @Foo 'Hello' @Bar 'World'"
        `parses` d "env" [] Nothing
          [st "Foo" $ Just "Hello", st "Bar" $ Just "World"]
        `leaving` ""
    it "with arguments" $
      p "env (a : String) (b : String) (c : String) => "
        `parses` d "env" [
            Arg "a" TyString, Arg "b" TyString, Arg "c" TyString
          ] Nothing []
        `leaving` ""
    it "with verb inner" $
      p "env # Verb => "
        `parses` d "env" [] (Just Verb) []
        `leaving` ""
    it "empty non-verb" $
      p "env #  => "
        `parses` d "env" [] (Just $ NonVerb $ Mode Nothing False) []
        `leaving` ""
    it "with non-verb inner + Math" $
      p "env # Math => "
        `parses` d "env" [] (Just $ NonVerb $ Mode (Just "Math") False) []
        `leaving` ""
    it "with non-verb inner + Math + noPref" $
      p "env # NoPref Math => "
        `parses` d "env" [] (Just $ NonVerb $ Mode (Just "Math") True) []
        `leaving` ""
    it "with non-verb inner + noPref" $
      p "env # NoPref => "
        `parses` d "env" [] (Just $ NonVerb $ Mode Nothing True) []
        `leaving` ""
    it "fail: empty" $
      p "" `failsLeaving` ""
  describe "Full definitions block" do
    let res =
          [ DefModeBlock
            [ DefMode "Simple"
            , DefMode "Math"
            ]
          , DefInModeBlock "Simple"
              [ DefEnvBlock
                [ DefEnv "Foo" [] Nothing
                    [ st "Begin" $ Just "Foo begin"
                    , st "End" $ Just "Foo end"
                    ]
                , DefEnv "Bar"
                    [ Arg "x" TyString
                    , Arg "y" TyString
                    ]
                    Nothing
                    [ st "End" $ Just $ "Bar " +> x ]
                , DefEnv "Baz" [] (Just $ NonVerb $ Mode "Math" False) []
                , DefEnv "Tmp" [] (Just $ NonVerb $ Mode "Math" True) []
                , DefEnv "Verb" [] (Just Verb) []
                ]
              , DefPrefBlock
                [ DefPref "" [pmel "" ">"] Nothing
                  [ st "Begin" ""
                  , st "Sep" "\\\\"
                  ]
                ]
              ]
          ]
    it "simple" $
      prs' pDefinitions
        [r|
@Define
  @Modes
    Simple
    Math
  @In Simple
    @Environments
      Foo => @Begin "Foo begin" @End "Foo end"
      Bar (x : String) (y : String) => @End "Bar " + x
      Baz # Math =>
      Tmp # NoPref Math =>
      Verb # Verb =>
    @Prefs
      ">" => @Begin @Sep "\\"
        |]
        `parses` res
        `leaving` ""
    it "lineFolded" $
      prs' pDefinitions
        [r|
@Define
  @Modes
    Simple
    Math
  @In Simple
    @Environments
      Foo =>
        @Begin "Foo begin"
        @End "Foo end"
      Bar (x : String)
          (y : String)
            =>
              @End "Bar " + x
      Baz # Math =>
      Tmp # NoPref Math =>
      Verb # Verb =>
    @Prefs
      ">" =>
        @Begin @Sep "\\"
        |]
        `parses` res
        `leaving` ""
