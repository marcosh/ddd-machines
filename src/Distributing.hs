{-# LANGUAGE InstanceSigs #-}

module Distributing where

-- base
import Control.Arrow ( Kleisli(..) )

-- profunctors
import Data.Profunctor ( Profunctor )

class Profunctor p => Distributing p where
  distribute :: (Applicative f, Traversable f) => f (p a b) -> p (f a) (f b)

instance Distributing (->) where
  distribute :: (Applicative f, Traversable f) => f (a -> b) -> f a -> f b
  distribute = (<*>)

instance Monad m => Distributing (Kleisli m) where
  distribute :: (Applicative f, Traversable f) => f (Kleisli m a b) -> Kleisli m (f a) (f b)
  distribute ts = Kleisli (\fa -> sequenceA $ (runKleisli <$> ts) <*> fa)