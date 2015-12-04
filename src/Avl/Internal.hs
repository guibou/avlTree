module Avl.Internal where

data AvlTree t = Node t Int (AvlTree t) (AvlTree t) | Leaf deriving (Show)

depth :: AvlTree t -> Int
depth Leaf = -1
depth (Node _ d _ _) = d

updateDepth :: AvlTree t -> AvlTree t -> Int
updateDepth a b = 1 + max (depth a) (depth b)

-- | Rotate the tree on the right. This is a partial function
unsafeRotateRight :: AvlTree t -> AvlTree t
unsafeRotateRight (Node v _ (Node v' _ suba' subb') subb) = let newsubb = (Node v (updateDepth subb' subb) subb' subb)
                                                      in Node v' (updateDepth suba' newsubb) suba' newsubb
unsafeRotateRight _ = error "cannot rotate on the right"

-- | Rotate the tree on the left. This is a partial function
unsafeRotateLeft :: AvlTree t -> AvlTree t
unsafeRotateLeft (Node v _ suba (Node v' _ suba' subb')) = let newsuba = (Node v (updateDepth suba' suba) suba suba')
                                                     in Node v' (updateDepth newsuba subb') newsuba subb'
unsafeRotateLeft _ = error "cannot rotate on the left"

balanceFactor :: AvlTree t -> AvlTree t -> Int
balanceFactor suba subb = depth subb - depth suba

subBalanceFactor :: AvlTree t -> Int
subBalanceFactor Leaf = 0
subBalanceFactor (Node _ _ suba subb) = balanceFactor suba subb

balance :: t -> AvlTree t -> AvlTree t -> AvlTree t
balance v suba subb
  | eq < -1 = let subbalance = subBalanceFactor suba
              in unsafeRotateRight $ if subbalance <= 0
                                     then node
                                     else let suba' = unsafeRotateLeft suba
                                          in (Node v (updateDepth suba' subb) suba' subb)
  | eq > 1 = let subbalance = subBalanceFactor subb
             in unsafeRotateLeft $ if subbalance >= 0
                                   then node
                                   else let subb' = unsafeRotateRight subb
                                        in (Node v (updateDepth suba subb') suba subb')

  | otherwise = node
  where eq = balanceFactor suba subb
        node = Node v (updateDepth suba subb) suba subb

foldAvl :: (a -> t -> a) -> a -> AvlTree t -> a
foldAvl _ acc Leaf = acc
foldAvl op acc (Node v _ suba subb) = foldAvl op (foldAvl op (op acc v) suba) subb
