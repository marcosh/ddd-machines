{-# LANGUAGE TypeApplications #-}

module DoorSpec where

import DDD ( Aggregate(Aggregate), Policy(Policy), Projection(Projection) )
import Door ( DoorCommand(..), DoorEvent(..), counter, door, doorAggregateAndPolicy, doorOpensOnKnock, doorProcess )

import MealySpec ( checkMealy )

-- base
import Data.Functor.Identity ( Identity )
import Data.Monoid ( Sum(Sum) )
import Data.Semigroup ( Last(Last) )

-- hspec
import Test.Hspec (Spec, describe, it)

-- profunctor
import Data.Profunctor

spec :: Spec
spec =
  describe "Door" $ do
    describe "aggregate" $ do
      let (Aggregate doorAggregate) = door
      it "should transform commands in events" $ do
        checkMealy @Identity doorAggregate [] [Open] [Opened]
        checkMealy @Identity doorAggregate [] [Close] []
        checkMealy @Identity doorAggregate [] [Knock] [Knocked]
        checkMealy @Identity doorAggregate [] [Open, Open] [Opened]
        checkMealy @Identity doorAggregate [] [Open, Close] [Opened, Closed]
        checkMealy @Identity doorAggregate [] [Open, Knock] [Opened, Knocked]
        checkMealy @Identity doorAggregate [] [Close, Open] [Opened]
        checkMealy @Identity doorAggregate [] [Close, Close] []
        checkMealy @Identity doorAggregate [] [Close, Knock] [Knocked]
        checkMealy @Identity doorAggregate [] [Knock, Open] [Knocked, Opened]
        checkMealy @Identity doorAggregate [] [Knock, Close] [Knocked]
        checkMealy @Identity doorAggregate [] [Knock, Knock] [Knocked, Knocked]

    describe "policy" $ do
      let (Policy doorPolicy) = doorOpensOnKnock
      it "should transform events in commands" $ do
        checkMealy @Identity doorPolicy [] [Opened] []
        checkMealy @Identity doorPolicy [] [Closed] []
        checkMealy @Identity doorPolicy [] [Knocked] [Open]
        checkMealy @Identity doorPolicy [] [Opened, Opened] []
        checkMealy @Identity doorPolicy [] [Opened, Closed] []
        checkMealy @Identity doorPolicy [] [Opened, Knocked] [Open]
        checkMealy @Identity doorPolicy [] [Closed, Opened] []
        checkMealy @Identity doorPolicy [] [Closed, Closed] []
        checkMealy @Identity doorPolicy [] [Closed, Knocked] [Open]
        checkMealy @Identity doorPolicy [] [Knocked, Opened] [Open]
        checkMealy @Identity doorPolicy [] [Knocked, Closed] [Open]
        checkMealy @Identity doorPolicy [] [Knocked, Knocked] [Open, Open]

    describe "projection" $ do
      let (Projection doorProjection) = counter
      let lastDoorProjection = rmap (Last . Sum) doorProjection
      it "should transform events in counts" $ do
        checkMealy @Identity lastDoorProjection (Last 0) [Opened] (Last 1)
        checkMealy @Identity lastDoorProjection (Last 0) [Closed] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Knocked] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Opened, Opened] (Last 2)
        checkMealy @Identity lastDoorProjection (Last 0) [Opened, Closed] (Last 1)
        checkMealy @Identity lastDoorProjection (Last 0) [Opened, Knocked] (Last 1)
        checkMealy @Identity lastDoorProjection (Last 0) [Closed, Opened] (Last 1)
        checkMealy @Identity lastDoorProjection (Last 0) [Closed, Closed] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Closed, Knocked] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Knocked, Opened] (Last 1)
        checkMealy @Identity lastDoorProjection (Last 0) [Knocked, Closed] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Knocked, Knocked] (Last 0)
        checkMealy @Identity lastDoorProjection (Last 0) [Opened, Opened, Opened] (Last 3)

    describe "aggregate and policy" $ do
      it "should transform commands in events using the feedback loop" $ do
        checkMealy @Identity doorAggregateAndPolicy [] [Open] [Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Close] []
        checkMealy @Identity doorAggregateAndPolicy [] [Knock] [Knocked, Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Open, Open] [Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Open, Close] [Opened, Closed]
        checkMealy @Identity doorAggregateAndPolicy [] [Open, Knock] [Opened, Knocked]
        checkMealy @Identity doorAggregateAndPolicy [] [Close, Open] [Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Close, Close] []
        checkMealy @Identity doorAggregateAndPolicy [] [Close, Knock] [Knocked, Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Knock, Open] [Knocked, Opened]
        checkMealy @Identity doorAggregateAndPolicy [] [Knock, Close] [Knocked, Opened, Closed]
        checkMealy @Identity doorAggregateAndPolicy [] [Knock, Knock] [Knocked, Opened, Knocked]

    describe "whole process" $ do
      let (Projection doorProjection) = counter
          lastDoorProjection = rmap (Last . Sum) doorProjection
          doorProcess' = doorProcess (Last 0) doorAggregateAndPolicy lastDoorProjection
      it "should transform commands in counts" $ do
        checkMealy @Identity doorProcess' (Last 0) [Open] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Close] (Last 0)
        checkMealy @Identity doorProcess' (Last 0) [Knock] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Open, Open] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Open, Close] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Open, Knock] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Close, Open] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Close, Close] (Last 0)
        checkMealy @Identity doorProcess' (Last 0) [Close, Knock] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Knock, Open] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Knock, Close] (Last 1)
        checkMealy @Identity doorProcess' (Last 0) [Knock, Knock] (Last 1)
