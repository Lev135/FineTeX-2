{-|
  This module contains types for storing FineTeX's source code structure
-}
module Language.FineTeX.Source.Syntax where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.FineTeX.Utils (Info)

data Document i = Document
  { definitions :: Maybe (DefBlock i)
  , body        :: [DocElement i]
  }
  deriving (Eq, Generic, Ord, Show)


-- * Imports (TODO)

-- * Definitions
newtype DefBlock i
  = DefBlock [DefSubBlock i]
  deriving (Eq, Generic, Ord, Show)

data DefSubBlock i
  = DefModeBlock [DefMode i]
  | DefInModeBlock (Info i Text) [DefInModeBlock i]
  deriving (Eq, Generic, Ord, Show)

newtype DefMode i
  = DefMode { name :: Info i Text }
  deriving (Eq, Generic, Ord, Show)

data DefInModeBlock i
  = DefEnvBlock [DefEnv i]
  | DefPrefBlock [DefPref i]
  deriving (Eq, Generic, Ord, Show)

-- | @name {args} ["#" inner] "=>" {process}@
data DefEnv i = DefEnv
  { name    :: Info i Text
  , args    :: [PatMatchExp i]
  , inner   :: Maybe (EnvInner i)
  , process :: [ProcStatement i]
  }
  deriving (Eq, Generic, Ord, Show)

-- | @"#Verb" | mode@
data EnvInner i
  = Verb
  | NonVerb (Mode i)
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
  , expr      :: PatMatchExp i
  , innerMode :: Maybe (Mode i)
  , process   :: [ProcStatement i]
  }
  deriving (Eq, Generic, Ord, Show)

-- ** Primitives

newtype PatMatchExp i
  = PatMatchExp [PatMatchEl i]
  deriving (Eq, Generic, Ord, Show)

data PatMatchEl i = PatMatchEl
  { varName  :: Maybe (Info i Text)
  , matchExp :: RegExp i
  }
  deriving (Eq, Generic, Ord, Show)

data ProcStatement i = ProcStatement
  { name    :: Info i Text
  , procRes :: Maybe (Exp i)
  }
  deriving (Eq, Generic, Ord, Show)

data RegExp i
  = REEmpty
  | REVoid
  | REName (Info i Text)
  | RECharSet (CharSetExp i)
  | RESeq (RegExp i) (RegExp i)
  | REOr (RegExp i) (RegExp i)
  | REBehind (CharSetExp i) (RegExp i)
  | REAhead (RegExp i) (CharSetExp i)
  deriving (Eq, Generic, Ord, Show)

data CharSetExp i
  = SName (CharSetExp i)
  | SPositive [CharRange]
  | SNegative [CharRange]
  | SUnion (CharSetExp i) (CharSetExp i)
  | SIntersection (CharSetExp i) (CharSetExp i)
  | SDifference (CharSetExp i) (CharSetExp i)
  deriving (Eq, Generic, Ord, Show)

type CharRange = (Char, Char)

data Exp i
  = EStringLit (Info i Text)
  | EIdent (Info i Text)
  | EApp (Exp i) (Exp i)
  deriving (Eq, Generic, Ord, Show)


-- * Body
data DocElement i
  = DocParLine [Info i WordOrSpace]
  | DocEnvironment (Info i Text) [Info i ArgStr] (EnvBody i DocElement)
  | DocPref (Info i Text) (Maybe ArgStr) [DocElement i]
  | DocEmptyLine
  deriving (Eq, Generic, Ord, Show)

type ArgStr = Text

data WordOrSpace
  = Word Text
  | Space
  deriving (Eq, Generic, Ord, Show)

data EnvBody i el
  = VerbBody [Info i Text]
  | NonVerbBody [el i]
  deriving (Eq, Generic, Ord, Show)
