{-# LANGUAGE OverloadedLists #-}
module Source.PreprocessSpec where
import Test.Hspec

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, runState)
import Data.Map (Map)
import Data.Text (Text)
import Language.FineTeX.Source.Errors
import Language.FineTeX.Source.Preprocess
import Language.FineTeX.Source.Syntax


type PreprocM = ExceptT [SourceError] (State Definitions)

defs :: Map Text DefMode -> Map Text InModeDefs -> Definitions
defs = Definitions

run :: (x -> PreprocM a) -> x -> (Either [SourceError] a, Definitions)
run ma x = runExceptT (ma x) `runState` defs mempty mempty

ok :: Either a ()
ok = Right ()

spec :: Spec
spec = do
  describe "preprocDefs" do
    it "empty" $
      run preprocDefs [] `shouldBe` (ok, defs [] [])
    it "modes" $
      run preprocDefs [ DefModeBlock [DefMode "Simple", DefMode "Math"] ]
        `shouldBe` (ok,
          defs [ ("Simple", DefMode "Simple"),
                 ("Math", DefMode "Math")
               ] [])
    it "multiple modes declaration" $
      run preprocDefs
          [ DefModeBlock
            [ DefMode "Simple"
            , DefMode "Math"
            , DefMode "Simple"
            , DefMode "Tmp"
            ]
          ]
        `shouldBe` (
          Left [DuplicateDef TyDefMode "Simple" Nothing],
          defs
            [ ("Simple", DefMode "Simple")
            , ("Math", DefMode "Math")
            , ("Tmp", DefMode "Tmp")
            ]
            []
          )
    it "environments with the same name in different modes" $
      run preprocDefs
        [ DefInModeBlock "A" [ DefEnvBlock [ DefEnv "E" [] Nothing [] ] ]
        , DefInModeBlock "B" [ DefEnvBlock [ DefEnv "E" [] Nothing [] ] ]
        ] `shouldBe` (
          ok,
          defs []
            [ ("A", InModeDefs Nothing [("E", DefEnv "E" [] Nothing [])] [])
            , ("B", InModeDefs Nothing [("E", DefEnv "E" [] Nothing [])] [])
            ]
        )
