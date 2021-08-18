{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module DDD where

import Mealy ( Mealy, MealyT )

newtype Aggregate command event = Aggregate (Mealy command [event])

data Policy m event command where
  Policy :: Monad m => MealyT m event [command] -> Policy m event command

newtype Projection event state = Projection (Mealy event state)
