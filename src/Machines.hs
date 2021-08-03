module Machines where

import Control.Arrow ( Arrow(first) )

-- base
import Data.Foldable ( foldlM )

-- machines
import Data.Machine.Mealy ( Mealy(..) )
import Data.Machine.MealyT ( MealyT(..) )

feedback :: Mealy c [e] -> Mealy e [c] -> Mealy c [e]
feedback p q = Mealy $ \c ->
  let
    -- the aggregate runs and produces some events and a new version of itself
    (es, p') = runMealy p c

    -- the process manager handles the events producing more commands and a new version of itself
    (cs, q') = foldl (\(cs', q'') e -> first (cs' <>) $ runMealy q'' e) ([], q) es

    -- we build the new version of the feedback machine
    feedback' = feedback p' q'

    -- we process the commands with the new feedback machine producing more events and a new version of the feedback machine
    (es', feedback'') = foldl (\(es'', feedback''') c' -> first (es'' <>) $ runMealy feedback''' c') ([], feedback') cs
  in
    (es <> es', feedback'')

feedbackM :: Monad m => MealyT m c [e] -> MealyT m e [c] -> MealyT m c [e]
feedbackM p q = MealyT $ \c -> do
  -- the aggregate runs and produces some events and a new version of itself
  (es, p') <- runMealyT p c

  -- the process manager handles the events producing more commands and a new version of itself
  (cs, q') <- foldlM (\(cs', q'') e -> first (cs' <>) <$> runMealyT q'' e) ([], q) es

  -- we build the new version of the feedback machine
  let feedbackM' = feedbackM p' q'

  -- we process the commands with the new feedback machine producing more events and a new version of the feedback machine
  (es', feedbackM'') <- foldlM (\(es'', feedbackM''') c' -> first (es'' <>) <$> runMealyT feedbackM''' c') ([], feedbackM') cs

  pure (es <> es', feedbackM'')
