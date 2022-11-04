module Language.FineTeX.Source.Errors where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.FineTeX.Utils
import qualified Prettyprinter as P
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- | The type of definition. Is used to unify the way of reporting
-- errors
data DefTy = TyDefMode | TyDefEnv | TyDefPref deriving (Eq, Generic, Ord, Show)
instance P.Pretty DefTy where
  pretty = \case
    TyDefMode -> "mode"
    TyDefEnv  -> "environment"
    TyDefPref -> "pref"

-- | Parsing and preprocessing errors
data SourceError
  = ParseError (ParseErrorBundle Text Void)
  | DuplicateDef DefTy (Posed Text) (Maybe Pos)

instance PrettyErr SourceError where
  prettyErr = \case
    ParseError peb
      -> [ nopos $ P.pretty $ errorBundlePretty peb ]
    DuplicateDef ty pname moldpos
      -> [ pname <&> \name ->
              P.hsep  [ "Duplicate definition of"
                      , P.pretty ty
                      , P.pretty $ show name
                      ]
         , case moldpos of
            Just oldpos ->
              pos oldpos "previously defined here"
            Nothing     ->
              nopos "unable to show previous location of previous definition"
         ]
