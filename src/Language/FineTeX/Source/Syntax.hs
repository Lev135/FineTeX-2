{-|
  This module contains types for storing FineTeX's source code structure
-}
module Language.FineTeX.Source.Syntax where

import Data.Function (on)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Range of source offsets, where element is situated
type Pos = Maybe (Int, Int)

data Posed a = Posed
  { getPos :: Pos
  , getVal :: a
  }
  deriving (Generic)

instance Eq a => Eq (Posed a) where
  (==) = (==) `on` getVal
instance Ord a => Ord (Posed a) where
  compare = compare `on` getVal
instance Show a => Show (Posed a) where
  show = show . getVal

type PText = Posed Text

data Document = Document
  { definitions :: Maybe DefBlock
  , body        :: [DocElement]
  }
  deriving (Eq, Generic, Ord, Show)


-- * Imports (TODO)

-- * Definitions
newtype DefBlock
  = DefBlock [DefSubBlock]
  deriving (Eq, Generic, Ord, Show)

data DefSubBlock
  = DefModeBlock [DefMode]
  | DefInModeBlock PText [DefInModeBlock]
  deriving (Eq, Generic, Ord, Show)

newtype DefMode
  = DefMode { name :: PText }
  deriving (Eq, Generic, Ord, Show)

data DefInModeBlock
  = DefEnvBlock [DefEnv]
  | DefPrefBlock [DefPref]
  deriving (Eq, Generic, Ord, Show)

-- | @name {args} ["#" inner] "=>" {process}@
data DefEnv = DefEnv
  { name    :: PText
  , args    :: [PatMatchExp]
  , inner   :: Maybe EnvInner
  , process :: [ProcStatement]
  }
  deriving (Eq, Generic, Ord, Show)

-- | @"#Verb" | mode@
data EnvInner
  = Verb
  | NonVerb Mode
  deriving (Eq, Generic, Ord, Show)

-- | @[\"NoPref"] [modeName]@
data Mode = Mode
  { modeName :: Maybe PText
  , noPref   :: Bool
  }
  deriving (Eq, Generic, Ord, Show)


-- | @[name:] expr [# innerMode] => {process}@
data DefPref = DefPref
  { name      :: Maybe PText
  , expr      :: PatMatchExp
  , innerMode :: Maybe Mode
  , process   :: [ProcStatement]
  }
  deriving (Eq, Generic, Ord, Show)

-- ** Primitives

newtype PatMatchExp
  = PatMatchExp [PatMatchEl]
  deriving (Eq, Generic, Ord, Show)

data PatMatchEl = PatMatchEl
  { varName  :: Maybe PText
  , matchExp :: RegExp
  }
  deriving (Eq, Generic, Ord, Show)

data ProcStatement = ProcStatement
  { name    :: PText
  , procRes :: Maybe Exp
  }
  deriving (Eq, Generic, Ord, Show)

data RegExp = REWord deriving (Eq, Generic, Ord, Show)

type CharRange = (Char, Char)

data Exp
  = EStringLit PText
  | EIdent PText
  | EApp Exp Exp
  | EBinOp Exp PText Exp
  | EPrefOp PText Exp
  | ESufOp Exp PText
  deriving (Eq, Generic, Ord, Show)

-- * Body
data DocElement
  = DocParLine [Posed WordOrSpace]
  | DocEnvironment PText [Posed ArgStr] (EnvBody DocElement)
  | DocPref PText (Maybe ArgStr) [DocElement]
  | DocEmptyLine
  deriving (Eq, Generic, Ord, Show)

type ArgStr = Text

data WordOrSpace
  = Word Text
  | Space
  deriving (Eq, Generic, Ord, Show)

data EnvBody el
  = VerbBody [PText]
  | NonVerbBody [el]
  deriving (Eq, Generic, Ord, Show)
