{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MealySpec where

import Mealy ( Mealy, MealyT, run, runMealyT, stateless, unfoldMealy )

-- base
import Data.Functor.Identity

-- hspec
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (arbitrary, forAll)

echo :: Monad m => MealyT m a [a]
echo = stateless pure

triangular :: Mealy Int [Int]
triangular = unfoldMealy update 0
  where
    update :: Int -> Int -> ([Int], Int)
    update state input = ([state + input], state + 1)

addIndex :: Int -> [Int] -> [Int]
addIndex _ []     = []
addIndex i (x:xs) = i + x : addIndex (i + 1) xs

checkMealy :: (Monad m, Eq (m b), Show (m b), Semigroup b) => MealyT m a b -> b -> [a] -> b -> Expectation
checkMealy mealy initial inputs output =
  (fst <$> run mealy initial inputs) `shouldBe` pure output

spec :: Spec
spec =
  describe "MealyT" $ do
    describe "runMealyT" $ do
      it "with echo machine should return the input" $ do
        forAll arbitrary $
          \(s :: String) -> (fst <$> runMealyT echo s) `shouldBe` Identity [s]

    describe "run" $ do
      it "with echo machine should return the inputs" $ do
        forAll arbitrary $
          \(ss :: [String]) -> checkMealy @Identity echo [] ss ss

      it "with triangular machine should return stateful results" $ do
        forAll arbitrary $
          \(is :: [Int]) -> checkMealy @Identity triangular [] is (addIndex 0 is)
