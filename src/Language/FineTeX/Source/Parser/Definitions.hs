{-# LANGUAGE RecordWildCards #-}
module Language.FineTeX.Source.Parser.Definitions where
import Data.Maybe (isJust)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Source.Syntax
import Language.FineTeX.Utils
import Text.Megaparsec
import Text.Megaparsec.Char

pDefinitions :: ParserM m => m (DefBlock PosOff)
pDefinitions = nonIndented $
  DefBlock <$ keyword "@Define"
    `headedMany` pDefSubBlock

pDefSubBlock :: ParserM m => m (DefSubBlock PosOff)
pDefSubBlock = choice
  [ DefModeBlock <$ keyword "@Modes"
      `headedMany` pDefMode
  , DefInModeBlock <$> (keyword "@In" *> ident)
      `headedMany` pDefInModeBlock
  ]

pDefMode :: ParserM m => m (DefMode PosOff)
pDefMode = DefMode <$> ident

pDefInModeBlock :: ParserM m => m (DefInModeBlock PosOff)
pDefInModeBlock = choice
  [ DefEnvBlock <$ keyword "@Environments"
      `headedMany` pDefEnv
  , DefPrefBlock <$ keyword "@Prefs"
      `headedMany` pDefPref
  ]

pDefEnv :: ParserM m => m (DefEnv PosOff)
pDefEnv = lineFold do
  name <- ident
  args <- many pArg
  inner <- optional do
    symbol "#"
    Verb <$ keyword "Verb" <|> NonVerb <$> pMode
  symbol "=>"
  process <- many pProcess
  pure DefEnv{..}

pDefPref :: ParserM m => m (DefPref PosOff)
pDefPref = lineFold do
  name <- optional $ ident <* symbol ":"
  expr <- pPatMatchExp
  innerMode <- optional do
    symbol "#"
    pMode
  symbol "=>"
  process <- many pProcess
  pure DefPref{..}

pArg :: ParserM m => m (PatMatchExp PosOff)
pArg = betweenSymbols "(" ")" pPatMatchExp

pPatMatchExp :: ParserM m => m (PatMatchExp PosOff)
pPatMatchExp = PatMatchExp <$> some pPatMatchEl

pPatMatchEl :: ParserM m => m (PatMatchEl PosOff)
pPatMatchEl =  (PatMatchEl Nothing <$> pRegExp)
    <|> betweenSymbols "(" ")"
          (PatMatchEl . Just <$> ident <* symbol ":" <*> pRegExp)

pRegExp :: m (RegExp PosOff)
pRegExp = todo

pMode :: ParserM m => m (Mode PosOff)
pMode = do
  noPref <- isJust <$> optional (keyword "NoPref")
  modeName <- optional ident
  pure Mode {..}

pProcess :: ParserM m => m (ProcStatement PosOff)
pProcess = do
  char '@'
  name <- ident
  exp <- optional pExp
  pure $ ProcStatement name exp

pExp :: m (Exp PosOff)
pExp = todo
