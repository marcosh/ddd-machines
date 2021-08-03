{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MachinesSpec where

import Machines

-- base
-- import Control.Applicative ( empty )

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- machines
import Data.Machine.Mealy
-- import Data.Machine.Plan
import Data.Machine.Process hiding (echo)
import Data.Machine.Source
import Data.Machine.Type

plus1upTo4 :: Mealy Int [Int]
plus1upTo4 = unfoldMealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ i =
      if   i < 5
      then ([i + 1], ())
      else ([], ())

echo :: Mealy Int [Int]
echo = unfoldMealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ i = ([i], ())

discard :: Mealy Int [Int]
discard = unfoldMealy update ()
  where
    update :: () -> Int -> ([Int], ())
    update _ _ = ([], ())

-- plus1upTo4 :: Process Int Int
-- plus1upTo4 = repeatedly $ do
--   i <- await
--   if i < 5
--   then yield $ i + 1
--   else empty

-- singleEcho :: Process Int Int
-- singleEcho = construct $ do
--   i <- await
--   yield i

foo :: Mealy Int [Int]
foo = unfoldMealy update 0
  where
    update :: Int -> Int -> ([Int], Int)
    update state input = ([state + input], state + 1)

-- test :: Process Int Int
-- test = stopped

spec :: Spec
spec =
  describe "Machines" $ do
    describe "feedback" $ do
      it "should process multiple inputs" $ do
        run (source [1 :: Int, 1] ~> auto (feedback echo discard)) `shouldBe` [[1], [1]]

      it "should use the feedback process" $ do
        run (source [1] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[1, 2, 3, 4, 5]]
        run (source [2] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[2, 3, 4, 5]]
        run (source [3] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[3, 4, 5]]
        run (source [4] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[4, 5]]
        run (source [5] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[5]]
        run (source [6] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[6]]
        run (source [1, 1] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]]
        run (source [1, 2] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[1, 2, 3, 4, 5], [2, 3, 4, 5]]
        run (source [3, 5, 1] ~> auto (feedback echo plus1upTo4)) `shouldBe` [[3, 4, 5], [5], [1, 2, 3, 4, 5]]

      it "should propagate the state" $ do
        take 10 <$> run (source [0] ~> auto (feedback foo echo)) `shouldBe` [[0, 1, 3, 6, 10, 15, 21, 28, 36, 45]]
