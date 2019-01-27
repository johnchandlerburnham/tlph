{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HList where

import           Data.Kind (Constraint, Type)
import           Data.List (intercalate)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

--instance Eq (HList '[]) where
--  (==) HNil HNil = True
--
--instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--  (==) (a :# as) (b :# bs) = a == b && as == bs
--
--instance Ord (HList '[]) where
--  compare HNil HNil = EQ
--
--instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--  compare (a :# as) (b :# bs) = case compare a b of
--    EQ -> compare as bs
--    x  -> x
--
--instance Show (HList '[]) where
--  show HNil = "'[]"
--
--instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--  show (a :# as) = show a ++ ":#" ++ show as

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  (==) HNil HNil           = True
  (==) (a :# as) (b :# bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = case compare a b of
    EQ -> compare as bs
    x  -> x

instance All Show ts => Show (HList ts) where
   show HNil        = "'[]"
   show (a :# HNil) = "'[" ++ show a ++ "]"
   show as          = "'[" ++ intercalate ", " (hShows as) ++ "]"

hShows :: All Show ts => HList ts -> [String]
hShows HNil      = []
hShows (a :# as) = show a : hShows as
