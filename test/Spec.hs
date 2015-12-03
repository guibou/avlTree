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

checkOrdering t = checkOrdering' t (const True)
  where checkOrdering' Leaf _ = True
        checkOrdering' (Node v _ suba subb) f = f v && checkOrdering' suba (<v) && checkOrdering' subb (>=v)

checkDepth Leaf = True
checkDepth node@(Node _ d suba subb) = d == recursiveDepth node && checkDepth suba && checkDepth subb

isLogarithmic :: AvlTree t -> Bool
isLogarithmic t = depth t <= floor (logBase 2 (fromIntegral (size t + 1)))

main :: IO ()
main = do
  -- check that insertion is ok and that it does not duplicate
  quickCheck ((\l -> size (fromList l) == length (nub l)) :: [Int] -> Bool)

  -- check idempotence of toList -> fromList
  quickCheck ((\l -> let t = fromList l in sort (nub l) == toList t) :: [Int] -> Bool)

  -- check that search works
  quickCheck ((\l0 -> let t = fromList l0 in all (search t) l0) :: [Int] -> Bool)

  -- check that search does not find items that are not here
  quickCheck ((\l0 l1 -> let t = fromList l0 in all (\x -> (search t x) == (x `elem` l0)) l1) :: [Int] -> [Int] -> Bool)

  -- check that cached depth works as it should
  quickCheck ((\l -> let t = fromList l in checkDepth t) :: [Int] -> Bool)

  -- check ordering property
  quickCheck ((\l -> let t = fromList l in checkOrdering t) :: [Int] -> Bool)

  -- check that depth is logarithm of the number of items
  quickCheck ((\l -> let t = fromList l in isLogarithmic t) :: [Int] -> Bool)
