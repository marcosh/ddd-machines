module App where

import DDD ( Projection(Projection) )
import Door
    ( counter,
      doorAggregateAndPolicy,
      doorProcess,
      DoorCommand,
      DoorOpenedCounter )
import Mealy ( run )

-- base
import Data.Monoid ( Sum(Sum) )
import Data.Semigroup ( Last(Last) )

-- profunctors
import Data.Profunctor ( Profunctor(rmap) )

app :: Monad m => [DoorCommand] -> m (Last (Sum DoorOpenedCounter))
app commands =
  let
    (Projection doorProjection) = counter
    doorMealy = doorProcess (Last 0) doorAggregateAndPolicy (rmap (Last . Sum) doorProjection)
  in
    fst <$> run doorMealy (Last 0) commands
