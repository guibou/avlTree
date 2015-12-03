module Avl.Internal where

data AvlTree t = Node t Int (AvlTree t) (AvlTree t) | Leaf deriving (Show)

depth :: AvlTree t -> Int
depth Leaf = -1
depth (Node _ d _ _) = d

updateDepth :: AvlTree t -> AvlTree t -> Int
updateDepth a b = 1 + max (depth a) (depth b)

foldAvl :: (a -> t -> a) -> a -> AvlTree t -> a
foldAvl _ acc Leaf = acc
foldAvl op acc (Node v _ suba subb) = foldAvl op (foldAvl op (op acc v) suba) subb
