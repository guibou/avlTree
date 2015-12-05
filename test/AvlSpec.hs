module AvlSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils

import Avl

import Data.List (nub, sort)

spec :: Spec
spec = do
  describe "Public API" $ do
    describe "singleton/empty + size/toList" $ do
      it "create an empty" $ do
        size empty `shouldBe` 0
        (toList empty :: [Int]) `shouldBe` []

      it "create a singleton" $ do
        property $ \x -> size (singleton (x :: Int)) === 1

      it "returns the right singleton" $ do
        property $ \x -> toList (singleton (x :: Int)) === [x]

    describe "fromList / toList / size" $ do
      it "check that insertion is ok and that it does not duplicate" $ do
        property $ \(IntList l) -> size (fromList l) === length (nub l)
  
      it "check idempotence of toList -> fromList" $ do
        property $ \(IntList l) -> let t = fromList l in sort (nub l) === toList t

    describe "search" $ do
      it "finds" $ do
        property $ \(IntList l0) -> let t = fromList l0 in all (search t) l0

      it "does not find"  $ do
        property $ \(IntList l0) (IntList l1) -> let t = fromList l0 in all (\x -> (search t x) == (x `elem` l0)) l1

    describe "insertions" $ do
      it "insert" $ do
        property $ \(IntAvlTree t) (IntList l) -> let t' = insertMany t l in toList t' === sort (nub (toList t ++ l))

      it "insertMany" $ do
        property $ \(IntAvlTree t) x -> let t' = insert t (x :: Int) in toList t' === sort (nub (x:toList t))

    describe "prettyPrint" $ do
      it "contains the numbers" $ do
        property $ \(IntAvlTree t) -> let l = map read (words (filter (/='.') (pprint t))) in sort l == toList t
