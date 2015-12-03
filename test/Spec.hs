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

checkOrdering :: AvlTree Int -> Bool
checkOrdering t = checkOrdering' t (const True)
  where checkOrdering' Leaf _ = True
        checkOrdering' (Node v _ suba subb) f = f v && checkOrdering' suba (<v) && checkOrdering' subb (>=v)

checkDepth :: AvlTree Int -> Bool
checkDepth Leaf = True
checkDepth node@(Node _ d suba subb) = d == recursiveDepth node && checkDepth suba && checkDepth subb

isLogarithmic :: AvlTree Int -> Bool
isLogarithmic t = depth t <= floor (logBase 2 (fromIntegral (size t + 1)))

canRotateLeft t = depthRight t > 0
canRotateRight t = depthLeft t > 0

instance (Arbitrary t, Ord t) => Arbitrary (AvlTree t) where
  arbitrary = fromList <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "check that insertion is ok and that it does not duplicate" $ do
      property ((\l -> size (fromList l)  == length (nub l)) :: [Int] -> Bool)
  
    it "check idempotence of toList -> fromList" $ do
      property ((\l -> let t = fromList l in sort (nub l) == toList t) :: [Int] -> Bool)

    it "check that search works" $ do
      property ((\l0 -> let t = fromList l0 in all (search t) l0) :: [Int] -> Bool)

    it "heck that search does not find items that are not here"  $ do
      property ((\l0 l1 -> let t = fromList l0 in all (\x -> (search t x) == (x `elem` l0)) l1) :: [Int] -> [Int] -> Bool)

    it "check that cached depth works as it should" $ do
      property checkDepth

    it "check ordering property" $ do
      property checkOrdering

    it "check that depth is logarithm of the number of items" $ do
      property isLogarithmic
