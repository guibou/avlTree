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

recursiveDepth :: AvlTree t -> Int
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

canRotateLeft :: AvlTree t -> Bool
canRotateLeft t = depthRight t > 0

canRotateRight :: AvlTree t -> Bool
canRotateRight t = depthLeft t > 0

matchTheAvlRule :: AvlTree t -> Bool
matchTheAvlRule Leaf = True
matchTheAvlRule (Node _ _ suba subb) = abs ((recursiveDepth suba) - (recursiveDepth subb)) <= 1 && matchTheAvlRule suba && matchTheAvlRule subb

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

  describe "left rotation" $ do
    it "keeps item" $ do
      property $ \(IntAvlTree t) -> canRotateLeft t ==> toList t === toList (unsafeRotateLeft t)
    it "keep ordering" $ do
      property $ \(IntAvlTree t) -> canRotateLeft t ==> checkOrdering (unsafeRotateLeft t)
    it "increses leftDepth" $ do
      property $ \(IntAvlTree t) -> canRotateLeft t ==> depthLeft (unsafeRotateLeft t) > depthLeft t
    it "decrease rightDepth" $ do
      property $ \(IntAvlTree t) -> canRotateLeft t ==> depthRight (unsafeRotateLeft t) < depthRight t

  describe "right rotation" $ do
    it "keeps item" $ do
      property $ \(IntAvlTree t) -> canRotateRight t ==> toList t === toList (unsafeRotateRight t)
    it "keep ordering" $ do
      property $ \(IntAvlTree t) -> canRotateRight t ==> checkOrdering (unsafeRotateRight t)
    it "increses rightDepth" $ do
      property $ \(IntAvlTree t) -> canRotateRight t ==> depthRight (unsafeRotateRight t) > depthRight t
    it "decrease leftDepth" $ do
      property $ \(IntAvlTree t) -> canRotateRight t ==> depthLeft (unsafeRotateRight t) < depthLeft t

  describe "insert" $ do
    it "insert" $ do
      property $ \(IntAvlTree t) x -> let t' = insert t (x :: Int) in toList t' === sort (nub (x:toList t))
    it "keep ordering" $ do
      property $ \(IntAvlTree t) x -> checkOrdering (insert t (x :: Int))
    it "keeps correct depth" $ do
      property $ \(IntAvlTree t) x -> checkDepth (insert t (x :: Int))
    it "keep log behavior" $ do
      property $ \(IntAvlTree t) x -> matchTheAvlRule (insert t (x :: Int))

  describe "insertMany" $ do
    it "insert" $ do
      property $ \(IntAvlTree t) (IntList l) -> let t' = insertMany t l in toList t' === sort (nub (toList t ++ l))
    it "keep ordering" $ do
      property $ \(IntAvlTree t) (IntList l)  -> checkOrdering (insertMany t l)
    it "keeps correct depth" $ do
      property $ \(IntAvlTree t) (IntList l) -> checkDepth (insertMany t l)
    it "keep log behavior" $ do
      property $ \(IntAvlTree t) (IntList l) -> matchTheAvlRule (insertMany t l)

