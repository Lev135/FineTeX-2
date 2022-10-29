{-|
  This module contains types for storing FineTeX's source code structure
-}
module Language.FineTeX.Source.Syntax where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.FineTeX.Utils (Info, WrapI)

data Document i = Document
  { definitions :: Maybe (DefBlock i)
  , body        :: [WrapI i DocElement]
  }
  deriving (Eq, Generic, Ord, Show)


-- * Imports (TODO)

-- * Definitions
newtype DefBlock i
  = DefinitionBlock [WrapI i DefSubBlock]
  deriving (Eq, Generic, Ord, Show)

data DefSubBlock i
  = DefModeBlock [WrapI i DefMode]
  | DefInModeBlock [WrapI i DefInModeBlock]
  deriving (Eq, Generic, Ord, Show)

newtype DefMode i
  = DefMode { name :: Info i Text }
  deriving (Eq, Generic, Ord, Show)

data DefInModeBlock i
  = DefEnvBlock [WrapI i DefEnv]
  | DefPrefBlock [WrapI i DefPref]
  deriving (Eq, Generic, Ord, Show)

-- | @name {args} ["#" inner] "=>" {process}@
data DefEnv i = DefEnv
  { name    :: Info i Text
  , args    :: [WrapI i PatMatchExp]
  , inner   :: Maybe (WrapI i EnvInner)
  , process :: [WrapI i ProcStatement]
  }
  deriving (Eq, Generic, Ord, Show)

-- | @"#Verb" | mode@
data EnvInner i
  = Verb
  | NoVerb (Mode i)
  deriving (Eq, Generic, Ord, Show)

-- | @[\"NoPref"] [modeName]@
data Mode i = Mode
  { modeName :: Maybe (Info i Text)
  , noPref   :: Bool
  }
  deriving (Eq, Generic, Ord, Show)


-- | @[name:] expr [# innerMode] => {process}@
data DefPref i = DefPref
  { name      :: Maybe (Info i Text)
  , expr      :: WrapI i PatMatchExp
  , innerMode :: Maybe (WrapI i Mode)
  , process   :: [WrapI i ProcStatement]
  }
  deriving (Eq, Generic, Ord, Show)

-- ** Primitives

newtype PatMatchExp i
  = PatMatchExp [WrapI i PatMatchEl]
  deriving (Eq, Generic, Ord, Show)

data PatMatchEl i = PatMatchEl
  { varName  :: Maybe (Info i Text)
  , matchExp :: WrapI i RegExp
  }
  deriving (Eq, Generic, Ord, Show)

data ProcStatement i = ProcStatement
  { name    :: Info i Text
  , procRes :: Maybe (WrapI i Exp)
  }
  deriving (Eq, Generic, Ord, Show)

data RegExp i
  = REEmpty
  | REVoid
  | REName (Info i Text)
  | RECharSet (WrapI i CharSetExp)
  | RESeq (WrapI i RegExp) (WrapI i RegExp)
  | REOr (WrapI i RegExp) (WrapI i RegExp)
  | REBehind (WrapI i CharSetExp) (WrapI i RegExp)
  | REAhead (WrapI i RegExp) (WrapI i CharSetExp)
  deriving (Eq, Generic, Ord, Show)

data CharSetExp i
  = SName (WrapI i CharSetExp)
  | SPositive [CharRange]
  | SNegative [CharRange]
  | SUnion (WrapI i CharSetExp) (WrapI i CharSetExp)
  | SIntersection (WrapI i CharSetExp) (WrapI i CharSetExp)
  | SDifference (WrapI i CharSetExp) (WrapI i CharSetExp)
  deriving (Eq, Generic, Ord, Show)

type CharRange = (Char, Char)

data Exp i
  = EStringLit (Info i Text)
  | EIdent (Info i Text)
  | EApp (WrapI i Exp) (WrapI i Exp)
  deriving (Eq, Generic, Ord, Show)


-- * Body
data DocElement i
  = DocParLine [Info i WordOrSpace]
  | DocEnvironment (Info i Text) [Info i ArgStr] (EnvBody i DocElement)
  | DocPref (Info i Text) (Maybe ArgStr) [WrapI i DocElement]
  | DocEmptyLine
  deriving (Eq, Generic, Ord, Show)

type ArgStr = Text

data WordOrSpace
  = Word Text
  | Space
  deriving (Eq, Generic, Ord, Show)

data EnvBody i el
  = VerbBody [Info i Text]
  | NonVerbBody [WrapI i el]
  deriving (Eq, Generic, Ord, Show)
