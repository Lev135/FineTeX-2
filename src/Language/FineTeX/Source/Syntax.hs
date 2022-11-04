{-|
  This module contains types for storing FineTeX's source code structure
-}
module Language.FineTeX.Source.Syntax where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.FineTeX.Utils


type PText = Posed Text

data Document = Document
  { definitions :: Maybe DefBlock
  , body        :: [DocElement]
  }
  deriving (Eq, Generic, Ord, Show)


-- * Imports (TODO)

-- * Definitions
type DefBlock = [DefSubBlock]

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
  , args    :: [Arg]
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
  , expr      :: [PatMatchEl]
  , innerMode :: Maybe Mode
  , process   :: [ProcStatement]
  }
  deriving (Eq, Generic, Ord, Show)

-- ** Primitives

data Arg = Arg
  { varName :: PText
  , varType :: Ty
  }
  deriving (Eq, Generic, Ord, Show)

data Ty = TyString deriving (Eq, Generic, Ord, Show)

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

data RegExp
  = REWord
  | REString PText
  deriving (Eq, Generic, Ord, Show)

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
