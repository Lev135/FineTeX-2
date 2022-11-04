{-# LANGUAGE DeriveDataTypeable #-}
{-|
  This module contains types for storing FineTeX's source code structure
-}
module Language.FineTeX.Source.Syntax where

import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.FineTeX.Utils


type PText = Posed Text

data Document = Document
  { definitions :: [DefSubBlock]
  , body        :: [DocElement]
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- * Imports (TODO)

-- * Definitions
data DefSubBlock
  = DefModeBlock [DefMode]
  | DefInModeBlock PText [DefInModeBlock]
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

newtype DefMode
  = DefMode { name :: PText }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DefInModeBlock
  = DefEnvBlock [DefEnv]
  | DefPrefBlock [DefPref]
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | @name {args} ["#" inner] "=>" {process}@
data DefEnv = DefEnv
  { name    :: PText
  , args    :: [Arg]
  , inner   :: Maybe EnvInner
  , process :: [ProcStatement]
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | @"#Verb" | mode@
data EnvInner
  = Verb
  | NonVerb Mode
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | @[\"NoPref"] [modeName]@
data Mode = Mode
  { modeName :: Maybe PText
  , noPref   :: Bool
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- | @[name:] expr [# innerMode] => {process}@
data DefPref = DefPref
  { name      :: Maybe PText
  , expr      :: [PatMatchEl]
  , innerMode :: Maybe Mode
  , process   :: [ProcStatement]
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- ** Primitives

data Arg = Arg
  { varName :: PText
  , varType :: Ty
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Ty = TyString deriving (Data, Eq, Generic, Ord, Show, Typeable)

data PatMatchEl = PatMatchEl
  { varName  :: Maybe PText
  , matchExp :: RegExp
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data ProcStatement = ProcStatement
  { name    :: PText
  , procRes :: Maybe Exp
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data RegExp
  = REWord
  | REString PText
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

type CharRange = (Char, Char)

data Exp
  = EStringLit PText
  | EIdent PText
  | EApp Exp Exp
  | EBinOp Exp PText Exp
  | EPrefOp PText Exp
  | ESufOp Exp PText
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- * Body
data DocElement
  = DocParLine [Posed WordOrSpace]
  | DocEnvironment PText [Posed ArgStr] (EnvBody DocElement)
  | DocPref PText (Maybe ArgStr) [DocElement]
  | DocEmptyLine
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

type ArgStr = Text

data WordOrSpace
  = Word Text
  | Space
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data EnvBody el
  = VerbBody [PText]
  | NonVerbBody [el]
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- * Generic names for prefixes and pretty-printing

-- | Generic name for prefs. Should be unique
genericPrefName :: DefPref -> PText
genericPrefName DefPref{name, expr} =
  fromMaybe (ppExpr expr) name

-- | Pretty-print PatMatchExp (the list of `PatMatchEl`s).
-- Used for generic names
ppExpr :: [PatMatchEl] -> PText
ppExpr = fmap T.unwords . sequencePos . map ppPatMatchEl

-- | Pretty-print `PatMatchEl`. Used for generic names
ppPatMatchEl :: PatMatchEl -> Posed Text
ppPatMatchEl PatMatchEl{varName, matchExp} = case varName of
  (Just name) -> "(" <> name <> ": " <> ppMatchExp matchExp <> ")"
  Nothing     -> ppMatchExp matchExp

-- | Pretty-print `RegExp`. Used for generic names
ppMatchExp :: RegExp -> PText
ppMatchExp = \case
  REWord        -> "REWord"
  REString pstr -> pstr
