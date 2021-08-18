{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module Mealy where

-- base
import Control.Arrow ( Arrow(first) )
import Data.Bifunctor ( bimap )
import Data.Foldable ( foldlM )

-- profunctors
import Data.Profunctor ( Profunctor(lmap, rmap) )

newtype MealyT m a b = MealyT { runMealyT :: a -> m (b, MealyT m a b) }

instance Functor m => Profunctor (MealyT m) where
  rmap :: (b -> c) -> MealyT m a b -> MealyT m a c
  rmap f (MealyT runMealy) = MealyT ((bimap f (rmap f) <$>) . runMealy)

  lmap :: (a -> b) -> MealyT m b c -> MealyT m a c
  lmap f (MealyT runMealy) = MealyT (((lmap f <$>) <$>) . runMealy . f)

unfoldMealyT :: Functor m => (s -> a -> m (b, s)) -> s -> MealyT m a b
unfoldMealyT f = go where
  go s = MealyT (fmap (go <$>) . f s)

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
run mealy initial = foldlM (\(b, mealy') a -> first (b <>) <$> runMealyT mealy' a) (initial, mealy)
