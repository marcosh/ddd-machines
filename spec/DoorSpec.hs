module DoorSpec where

import Door

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- machines
import Data.Machine.Process
import Data.Machine.Source
import Data.Machine.Type

spec :: Spec
spec =
  describe "Door" $ do
    describe "door" $ do
      let (Aggregate doorAggregateProcess) = door
      it "should work as expected as a process" $ do
        run (doorAggregateProcess <~ source [Open]) `shouldBe` [(IsOpen, Opened)]
        run (doorAggregateProcess <~ source [Close]) `shouldBe` []
        run (doorAggregateProcess <~ source [Knock]) `shouldBe` [(IsClosed, Knocked)]
        run (doorAggregateProcess <~ source [Open, Open]) `shouldBe` [(IsOpen, Opened)]
        run (doorAggregateProcess <~ source [Open, Close]) `shouldBe` [(IsOpen, Opened), (IsClosed, Closed)]
        run (doorAggregateProcess <~ source [Open, Knock]) `shouldBe` [(IsOpen, Opened), (IsOpen, Knocked)]
