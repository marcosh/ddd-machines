{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Mealy where

import Stateful ( Stateful(..) )

-- base
import Control.Arrow ( Arrow(..), Kleisli(..) )
import Data.Foldable ( foldlM )

{- | MealyT ~ (Kleisli m) a (b, MealyT m a b)
            ~ a -> m (b, MealyT m a b)
 -}
type MealyT m a b = Stateful (Kleisli m) a b

mealyT :: (a -> m (b, MealyT m a b)) -> MealyT m a b
mealyT = Stateful . Kleisli

runMealyT :: MealyT m a b -> a -> m (b, MealyT m a b)
runMealyT (Stateful (Kleisli f)) = f

unfoldMealyT :: Functor m => (s -> a -> m (b, s)) -> s -> MealyT m a b
unfoldMealyT f s = mealyT $ ((unfoldMealyT f <$>) <$>) . f s

type Mealy a b = forall m . Monad m => MealyT m a b

unfoldMealy :: (s -> a -> (b, s)) -> s -> Mealy a b
unfoldMealy f = unfoldMealyT ((pure .) . f)

unfoldMoore :: (s -> (b, a -> s)) -> s -> Mealy a b
unfoldMoore f = unfoldMealy (\s a -> ($ a) <$> f s)

stateless :: (a -> b) -> Mealy a b
stateless f = unfoldMealy (\s a -> (f a, s)) ()

{- | Iteratively passes a sequence of arguments to a machine accumulating the results in a Monoid.
It returns also a new version of the machine with the status updated after all the applications.
-}
run :: (Monad m, Semigroup b, Foldable f) => MealyT m a b -> b -> f a -> m (b, MealyT m a b)
run mealy initial = foldlM
  (\(b, mealy') a -> first (b <>) <$> runMealyT mealy' a)
  (initial, mealy)
