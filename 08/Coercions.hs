{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Coercions where

import           Data.Coerce   (Coercible (..), coerce)
import           Data.Foldable (toList)
import qualified Data.Map      as M
import           Data.Monoid   (Product (..), Sum (..))

newtype ZipList a = ZipList { getZipList :: [a] }

newtype Sum' a = Sum' { getSum' :: a }

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

newtype Reverse a = Reverse { getReverse :: a } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

coerceExample :: M.Map Char (Reverse Bool)
coerceExample = coerce (M.singleton 'S' True)

--coerceExampleBad :: M.Map (Reverse Char) Bool
--coerceExampleBad = coerce (M.singleton 'S' True)

data BST v = Empty | Branch (BST v) v (BST v)

type role BST nominal
