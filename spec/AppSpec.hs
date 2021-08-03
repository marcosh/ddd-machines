module AppSpec where

-- import App
-- import Door

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "App" $ do
    describe "app" $ do
      it "" $ do
        True `shouldBe` True

      -- it "should count 0 if I close the door once" $ do
      --   app [Close] `shouldBe` [0]

      -- it "should count 0 if I close the door several times" $ do
      --   app [Close, Close, Close, Close] `shouldBe` [0]

      -- it "should count 1 if I open the door once" $ do
      --   app [Open] `shouldBe` [0, 1]

      -- it "should count 1 if I open the door several times" $ do
      --   app [Open, Open, Open, Open] `shouldBe` [0, 1]

      -- it "should count 1 if I knock once" $ do
      --   app [Knock] `shouldBe` [0, 0, 1]

      -- it "should count 1 if I knock several times" $ do
      --   take 10 (appAggregateAndPolicy [Open, Close]) `shouldBe` []
