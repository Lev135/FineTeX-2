{-# LANGUAGE RecordWildCards #-}
module Language.FineTeX.Source.Parser.Definitions where

import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Maybe (isJust)
import Language.FineTeX.Source.Parser.Utils
import Language.FineTeX.Source.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char

pDefinitions :: ParserM m => m [DefSubBlock]
pDefinitions = nonIndented $
  id <$ keyword "@Define"
    `headedMany` pDefSubBlock

pDefSubBlock :: ParserM m => m DefSubBlock
pDefSubBlock = choice
  [ DefModeBlock <$ keyword "@Modes"
      `headedMany` pDefMode
  , DefInModeBlock <$> (keyword "@In" *> ident)
      `headedMany` pDefInModeBlock
  ]

pDefMode :: ParserM m => m DefMode
pDefMode = DefMode <$> ident

pDefInModeBlock :: ParserM m => m DefInModeBlock
pDefInModeBlock = choice
  [ DefEnvBlock <$ keyword "@Environments"
      `headedMany` pDefEnv
  , DefPrefBlock <$ keyword "@Prefs"
      `headedMany` pDefPref
  ]

pDefEnv :: ParserM m => m DefEnv
pDefEnv = lineFold do
  name <- ident
  args <- many pArg
  inner <- optional do
    symbol "#"
    Verb <$ keyword "Verb" <|> NonVerb <$> pMode
  symbol "=>"
  process <- many pProcess
  pure DefEnv{..}

pDefPref :: ParserM m => m DefPref
pDefPref = lineFold do
  name <- optional $ ident <* symbol ":"
  expr <- many pPatMatchEl
  innerMode <- optional do
    symbol "#"
    pMode
  symbol "=>"
  process <- many pProcess
  pure DefPref{..}

pArg :: ParserM m => m Arg
pArg = betweenSymbols "(" ")" (Arg <$> ident <* symbol ":" <*> pTy)

pTy :: ParserM m => m Ty
pTy = TyString <$ keyword "String"

pPatMatchEl :: ParserM m => m PatMatchEl
pPatMatchEl =  (PatMatchEl Nothing <$> pRegExp)
    <|> betweenSymbols "(" ")"
          (PatMatchEl . Just <$> ident <* symbol ":" <*> pRegExp)

pRegExp :: ParserM m => m RegExp
pRegExp
  = REWord <$ keyword "Word"
  <|> REString <$> stringLit

pMode :: ParserM m => m Mode
pMode = do
  noPref <- isJust <$> optional (keyword "NoPref")
  modeName <- optional ident
  pure Mode {..}

pProcess :: ParserM m => m ProcStatement
pProcess = do
  char '@'
  name <- ident
  exp <- optional pExp
  pure $ ProcStatement name exp

pExp :: ParserM m => m Exp
pExp = makeExprParser pTerm operators
  where
    pTerm = choice
      [ EStringLit <$> stringLit
      , EIdent <$> ident
      , betweenSymbols "(" ")" pExp]
    operators =
      [ [ Prefix $ EPrefOp <$> located (symbol "!!")
        , Prefix $ EPrefOp <$> located (symbol "!")
        ]
      , [ InfixL $ pure EApp ]
      , [ InfixL $ flip EBinOp <$> located (symbol "+") ]
      ]
