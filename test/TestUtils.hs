module TestUtils where

import Test.QuickCheck
import Avl

newtype IntList = IntList [Int] deriving (Show)
newtype IntAvlTree = IntAvlTree (AvlTree Int) deriving (Show)

instance Arbitrary IntAvlTree where
  arbitrary = IntAvlTree . fromList <$> arbitrary

instance Arbitrary IntList where
  arbitrary = IntList <$> arbitrary

