{-# LANGUAGE RankNTypes #-}
module Cont where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

newtype Identity a = Identity { unIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . unIdentity

cont :: a -> Cont a
cont a = Cont (\f -> f a)

runCont :: Cont a -> a
runCont f = let callback = id in (unCont f) callback

g :: Cont a -> Identity a
g = Identity . runCont

g' :: Identity a -> Cont a
g' = cont . unIdentity

--instance Functor Cont where
--  fmap f = g' . (fmap f) . g

instance Functor Cont where
  fmap f = cont . f . runCont

instance Applicative Cont where
  pure = cont
  (<*>) cf ca = (runCont cf) <$> ca

instance Monad Cont where
  (>>=) c f = runCont $ f <$> c

class MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype ContT r m a = ContT
  { unContT :: Monad m => forall r. (a -> m r) -> m r
  }

contT :: Monad m => m a -> ContT r m a
contT ma = ContT (\f -> f =<< ma)

instance MonadTrans (ContT r) where
  lift = contT

