{-# LANGUAGE TypeApplications #-}

module MachinesSpec where

import Machines ( feedback )
import Mealy ( Mealy, run, unfoldMealy )

import MealySpec ( checkMealy, echo, triangular )

-- base
import Data.Functor.Identity ( Identity(Identity) )

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

plus1upTo4 :: Mealy Int [Int]
plus1upTo4 = unfoldMealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ i =
      if   i < 5
      then ([i + 1], ())
      else ([], ())

discard :: Mealy Int [Int]
discard = unfoldMealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ _ = ([], ())

spec :: Spec
spec =
  describe "Machines" $ do
    describe "feedback" $ do
      it "should process multiple inputs" $ do
        checkMealy @Identity (feedback echo discard) [] [1, 1] [1, 1]

      it "should use the feedback process" $ do
        checkMealy @Identity (feedback echo plus1upTo4) [] [1] [1, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [2] [2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [3] [3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [4] [4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [5] [5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [6] [6]
        checkMealy @Identity (feedback echo plus1upTo4) [] [1, 1] [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [1, 2] [1, 2, 3, 4, 5, 2, 3, 4, 5]
        checkMealy @Identity (feedback echo plus1upTo4) [] [3, 5, 1] [3, 4, 5, 5, 1, 2, 3, 4, 5]

      it "should propagate the state" $ do
        (take 10 . fst <$> run (feedback triangular echo) [] [0]) `shouldBe` Identity [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
