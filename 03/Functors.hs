{-# LANGUAGE KindSignatures #-}

module Functors where

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

class Covariant f where
  comap :: (a -> b) -> f a -> f b

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

newtype T1 a = T1 (Int -> a)

instance Covariant T1 where
  comap f (T1 g) = T1 $ f . g

newtype T2 a = T2 (a -> Int)

instance Contravariant T2 where
  contramap f (T2 g) = T2 $ g . f

newtype T3 a = T3 (a -> a)

instance Invariant T3 where
  invmap f f' (T3 g) = T3 $ f . g . f'

newtype T4 a = T4 ((Int -> a) -> Int)

instance Contravariant T4 where
  contramap f (T4 g) = T4 $ \h -> g (f . h)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Covariant T5 where
  comap f (T5 g) = T5 $ \h -> g (h . f)


