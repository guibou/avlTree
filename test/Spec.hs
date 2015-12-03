module Main where

import Test.Hspec
import Test.QuickCheck

import Avl
import Avl.Internal

import Data.List (nub, sort)

depthLeft :: AvlTree t -> Int
depthLeft Leaf = -1
depthLeft (Node _ _ l _) = depth l

depthRight :: AvlTree t -> Int
depthRight Leaf = -1
depthRight (Node _ _ _ r) = depth r

recursiveDepth Leaf = -1
recursiveDepth (Node _ _ suba subb) = 1 + max (recursiveDepth suba) (recursiveDepth subb)

checkOrdering :: Ord t => AvlTree t -> Bool
checkOrdering t = checkOrdering' t (const True)
  where checkOrdering' Leaf _ = True
        checkOrdering' (Node v _ suba subb) f = f v && checkOrdering' suba (<v) && checkOrdering' subb (>=v)

checkDepth :: AvlTree t -> Bool
checkDepth Leaf = True
checkDepth node@(Node _ d suba subb) = d == recursiveDepth node && checkDepth suba && checkDepth subb

isLogarithmic :: AvlTree t -> Bool
isLogarithmic t = depth t <= floor (logBase 2 (fromIntegral (size t + 1)))

canRotateLeft t = depthRight t > 0
canRotateRight t = depthLeft t > 0

newtype IntList = IntList [Int] deriving (Show)
newtype IntAvlTree = IntAvlTree (AvlTree Int) deriving (Show)

instance Arbitrary IntAvlTree where
  arbitrary = IntAvlTree . fromList <$> arbitrary

instance Arbitrary IntList where
  arbitrary = IntList <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "check that insertion is ok and that it does not duplicate" $ do
      property $ \(IntList l) -> size (fromList l) === length (nub l)
  
    it "check idempotence of toList -> fromList" $ do
      property $ \(IntList l) -> let t = fromList l in sort (nub l) === toList t

    it "check that search works" $ do
      property $ \(IntList l0) -> let t = fromList l0 in all (search t) l0

    it "heck that search does not find items that are not here"  $ do
      property $ \(IntList l0) (IntList l1) -> let t = fromList l0 in all (\x -> (search t x) == (x `elem` l0)) l1

    it "check that cached depth works as it should" $ do
      property $ \(IntAvlTree t) -> checkDepth t

    it "check ordering property" $ do
      property $ \(IntAvlTree t) -> checkOrdering t

    it "check that depth is logarithm of the number of items" $ do
      property $ \(IntAvlTree t) -> isLogarithmic t
