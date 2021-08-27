{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Stateful where

import Distributing ( Distributing(..) )

-- base
import Control.Arrow ( Arrow(second, (***), (&&&)) )
import qualified Control.Category as C ( Category(..) )
import qualified Data.List.NonEmpty as NE ( unzip )

-- product-profunctors
import Data.Profunctor.Product ( ProductProfunctor(..), SumProfunctor(..) )

-- profunctors
import Data.Profunctor ( Profunctor(..), Strong(..), Choice(..) )

newtype Stateful p a b = Stateful (p a (b, Stateful p a b))

instance Profunctor p => Profunctor (Stateful p) where
  lmap :: (a -> b) -> Stateful p b c -> Stateful p a c
  lmap f (Stateful s) = Stateful $ second (lmap f) `rmap` lmap f s

  rmap :: (a -> b) -> Stateful p c a -> Stateful p c b
  rmap f (Stateful s) = Stateful $ (f *** rmap f) `rmap` s

instance (C.Category p, Strong p) => C.Category (Stateful p) where
  id :: Stateful p a a
  id = Stateful $ (id &&& const C.id) `rmap` C.id

  (.) :: Stateful p b c -> Stateful p a b -> Stateful p a c
  (.) (Stateful s1) (Stateful s2) =
    Stateful $ ((\((c, s1'), s2') -> (c, s1' C.. s2')) `rmap` first' s1) C.. s2

instance Strong p => Strong (Stateful p) where
  first' :: Stateful p a b -> Stateful p (a, c) (b, c)
  first' (Stateful s) = Stateful $ (\((b, s'), c) -> ((b, c), first' s')) `rmap` first' s

  second' :: Stateful p a b -> Stateful p (c, a) (c, b)
  second' (Stateful s) = Stateful $ (\(c, (b, s')) -> ((c, b), second' s')) `rmap` second' s

instance Choice p => Choice (Stateful p) where
  left' :: Stateful p a b -> Stateful p (Either a c) (Either b c)
  left' (Stateful s) = Stateful $ (\case
    Left (b, s') -> (Left b, left' s')
    Right c -> (Right c, left' (Stateful s)))
      `rmap` left' s

  right' :: Stateful p a b -> Stateful p (Either c a) (Either c b)
  right' (Stateful s) = Stateful $ (\case
    Left c -> (Left c, right' (Stateful s))
    Right (b, s') -> (Right b, right' s'))
      `rmap` right' s

instance ProductProfunctor p => ProductProfunctor (Stateful p) where
  purePP b = Stateful $ (, purePP b) `rmap` purePP b

  (****) :: Stateful p a (b -> c) -> Stateful p a b -> Stateful p a c
  (****) (Stateful s1) (Stateful s2) =
    Stateful $ (\(f, s1') (b, s2') -> (f b, s1' **** s2')) `rmap` s1 **** s2

instance SumProfunctor p => SumProfunctor (Stateful p) where
  (+++!) :: Stateful p a b -> Stateful p a' b' -> Stateful p (Either a a') (Either b b')
  (+++!) (Stateful s1) (Stateful s2) =
    Stateful $ (\case
      Left  (b , s1') -> (Left  b , s1' +++! Stateful s2)
      Right (b', s2') -> (Right b', Stateful s1 +++! s2'))
        `rmap` (s1 +++! s2)

instance Distributing p => Distributing (Stateful p) where
  distribute :: (Applicative f, Traversable f) => f (Stateful p a b) -> Stateful p (f a) (f b)
  distribute fs =
    let
      unwrappedFs = (\(Stateful pab) -> pab) <$> fs
    in
      Stateful $ ((distribute <$>) . NE.unzip) `rmap` distribute unwrappedFs
