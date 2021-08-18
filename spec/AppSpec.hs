module AppSpec where

import App ( app )
import Door ( DoorCommand(..) )

-- base
import Data.Semigroup ( Last(Last) )

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "App" $ do
    describe "app" $ do
      it "should count 0 if I close the door once" $ do
        app [Close] `shouldBe` [Last 0]

      it "should count 0 if I close the door several times" $ do
        app [Close, Close, Close, Close] `shouldBe` [Last 0]

      it "should count 1 if I open the door once" $ do
        app [Open] `shouldBe` [Last 1]

      it "should count 1 if I open the door several times" $ do
        app [Open, Open] `shouldBe` [Last 1]

      it "should count 1 if I knock once" $ do
        app [Knock] `shouldBe` [Last 1]

      it "should count 1 if I knock several times" $ do
        app [Knock, Knock] `shouldBe` [Last 1]
