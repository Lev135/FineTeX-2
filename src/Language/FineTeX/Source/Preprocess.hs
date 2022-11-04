{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{- |
  This module provides preprocessing definitions after parsing.
-}
module Language.FineTeX.Source.Preprocess where

import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadWriter)
import Control.Monad.State (MonadState)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.FineTeX.Source.Errors
import Language.FineTeX.Source.Syntax
import Language.FineTeX.Utils
import Optics

-- | Collected definitions
data Definitions = Definitions
  { modes      :: Map Text DefMode
  , inModeDefs :: Map Text InModeDefs
  }
  deriving (Eq, Generic, Ord, Show)

-- | Collected definitions in particular mode
data InModeDefs = InModeDefs
  { -- | Position of the first block of definitions in this mode to report
  -- an "undefined mode error"
    inModeDefsPos :: Maybe Pos
    -- | Environments, defined in this mode
  , envs          :: Map Text DefEnv
    -- | Prefs, defined in this mode
  , prefs         :: Map Text DefPref
  }
  deriving (Eq, Generic, Ord, Show)

-- | Preprocess parsed definitions. Returns all definitions, including imported
-- ones
preprocDefs :: (MonadState Definitions m, MonadError [SourceError] m) =>
  [DefSubBlock] -> m ()
preprocDefs blocks = runErrorM $ preprocDefsImpl blocks

-- | Monad constraint for source preprocessing.
-- Provides a state with currently processed definitions and writer for logging
-- errors.
type PreprocDefsM m = (MonadState Definitions m, MonadWriter [SourceError] m)

-- | Monadic action for processing block of definitions
preprocDefsImpl :: PreprocDefsM m => [DefSubBlock] -> m ()
preprocDefsImpl = traverse_ \case
   DefModeBlock dmodes ->
    for_ dmodes $ insertDef TyDefMode #modes #name
   DefInModeBlock mode dinModes ->
    for_ dinModes (preprocInModeDef mode)

-- | Monadic action for processing @In <mode>@ block of definitions
preprocInModeDef :: PreprocDefsM m => PText -> DefInModeBlock -> m ()
preprocInModeDef (Posed p mode) = \case
  DefEnvBlock  denvs  ->
    for_ denvs $ insertDef TyDefEnv (inModeLens % #envs) #name
  DefPrefBlock dprefs ->
    for_ dprefs $ insertDef TyDefPref (inModeLens % #prefs) (to genericPrefName)
  where inModeLens = #inModeDefs % at mode % non (InModeDefs p mempty mempty)

-- | General action to insert a definition in map if it does not exist there
-- or report an `DuplicateDef` error
insertDef :: (PreprocDefsM m, _) =>
  -- | type of definition
  DefTy ->
  -- | lens to extract definitions' map
  Optic' k  NoIx Definitions (Map Text def) ->
  -- | getter to extract name from definition
  Optic' k' NoIx def PText ->
  -- | definition to insert
  def ->
  m ()
insertDef defTy defLens nameGetter def = do
  let name = def ^. nameGetter % #getVal
  moldPos <- preuse $ #modes % ix name % #name % #getPos
  case moldPos of
    Nothing -> defLens % at name `assign` Just def
    Just p' -> tellErr $ DuplicateDef defTy (def ^. nameGetter) p'
