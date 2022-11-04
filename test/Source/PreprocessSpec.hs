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
import Language.FineTeX.Utils (Pos(..), Posed, SourceId(..), getPos, pos)


type PreprocM = ExceptT [SourceError] (State Definitions)

defs :: Map Text DefMode -> Map Text InModeDefs -> Definitions
defs = Definitions

run :: (x -> PreprocM a) -> x -> (Either [SourceError] a, Definitions)
run ma x = runExceptT (ma x) `runState` defs mempty mempty

ok :: Either a ()
ok = Right ()

p0 :: (Int, Int) -> a -> Posed a
p0 p = pos (Pos (SourceId 0) p)

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
            [ DefMode $ p0 (1,  6)  "Simple"
            , DefMode $ p0 (10, 15) "Math"
            , DefMode $ p0 (20, 26) "Simple"
            , DefMode $ p0 (30, 40) "Tmp"
            ]
          ]
        `shouldBe` (
          Left [ DuplicateDef TyDefMode
                    (p0 (20, 26) "Simple")
                    (getPos $ p0 (1, 6) ())
               ],
          defs
            [ ("Simple", DefMode $ p0 (1, 6) "Simple")
            , ("Math", DefMode $ p0 (10, 15) "Math")
            , ("Tmp", DefMode $ p0 (30, 40) "Tmp")
            ]
            []
          )
    it "two blocks of definitions in the same mode" $
      run preprocDefs
        [ DefInModeBlock (p0 (1, 5) "A")
            [ DefEnvBlock [ DefEnv (p0 (6, 9) "E1") [] Nothing []]]
        , DefInModeBlock (p0 (10, 15) "A")
            [ DefEnvBlock [ DefEnv (p0 (16, 19) "E2") [] Nothing []]]
        ] `shouldBe` (ok,
          defs
            []
            [ ("A", InModeDefs (getPos $ p0 (1, 5) ())
                [ ("E1", DefEnv (p0 (6,  9)  "E1") [] Nothing [])
                , ("E2", DefEnv (p0 (16, 19) "E2") [] Nothing [])
                ] []
              )
            ]
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
