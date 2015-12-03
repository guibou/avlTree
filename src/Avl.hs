{- | Stores values in a Tree like structure for efficient O(n log n) set operations.

Values should be in the Ord class
-}

module Avl
       (AvlTree
       ,singleton
       ,empty
       ,search
       ,fromList
       ,toList
       ,pprint
       ,size
        )
       where

import Data.List (sort, nub)

import Avl.Internal

-- | Search a value in the tree
search :: Ord t => AvlTree t -> t -> Bool
search Leaf _ = False
search (Node v' _  suba subb) v
  | v < v' = search suba v
  | v > v' = search subb v
  | otherwise = True

-- | Converts a list to a tree. Duplicate elements are ignored
fromList :: Ord t => [t] -> AvlTree t
fromList [] = empty
fromList l = let sorted = sort (nub l)
                 (sla, (pivot:slb)) = splitAt (length (sorted) `div` 2) sorted
                 suba = fromList sla
                 subb = fromList slb
                 
             in Node pivot (updateDepth suba subb) suba subb

-- | Converts a tree to a list. Items are returned in order
toList :: AvlTree t -> [t]
toList Leaf = []
toList (Node v _ suba subb) = toList suba ++ [v] ++ toList subb

-- | Size of the tree. O(n)
size :: AvlTree t -> Int
size = foldAvl (\x _ -> x + 1) 0

-- | Pretty printing version of the tree
pprint :: Show t => AvlTree t -> String
pprint = unlines . pprint'
  where
    pprint' :: Show t => AvlTree t -> [String]
    pprint' Leaf = ["."]
    pprint' (Node v _ suba subb) = subbp ++ [sv] ++ subap
      where
        sv = show v
        l = length sv + 3
        paddingStr = replicate l ' '
        subap = map (paddingStr ++) (pprint' suba)
        subbp = map (paddingStr ++) (pprint' subb)

-- | Empty Set
empty :: AvlTree t
empty = Leaf

-- | Set with one item
singleton :: t -> AvlTree t
singleton v = Node v 0 Leaf Leaf
