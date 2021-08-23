{-# LANGUAGE Rank2Types #-}

module Machines where

import Mealy ( MealyT, mealyT, run, runMealyT )

feedback :: Monad m => MealyT m c [e] -> MealyT m e [c] -> MealyT m c [e]
feedback p q = mealyT $ \c -> do
  -- the aggregate runs and produces some events and a new version of itself
  (es, p') <- runMealyT p c

  -- the process manager handles the events producing more commands and a new version of itself
  (cs, q') <- run q [] es

  -- we build the new version of the feedback machine
  let feedback' = feedback p' q'

  -- we process the commands with the new feedback machine producing more events and a new version of the feedback machine
  (es', feedback'') <- run feedback' [] cs

  pure (es <> es', feedback'')
