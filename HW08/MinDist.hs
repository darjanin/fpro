module MinDist where
import TypyMinDist
-- data TreeBST a = EmptyBST | NodeBST a (TreeBST a) (TreeBST a)
--    deriving (Show, Read)
minDistanceBST :: (Ord a) => a -> a -> TreeBST a -> Int
minDistanceBST x y t | not (includes x t) || not (includes y t) = -1
-- minDistanceBST x y t = distance x t + distance y t - 2 * distance lca t
minDistanceBST x y _ | x == y = 0
minDistanceBST x y t = distance x pt + distance y pt - 1
  where pt = parent x y t

t = NodeBST 6 (NodeBST 3 (NodeBST 2 (NodeBST 1 EmptyBST EmptyBST) EmptyBST) (NodeBST 5 (NodeBST 4 EmptyBST EmptyBST) EmptyBST)) (NodeBST 9 (NodeBST 8 (NodeBST 7 EmptyBST EmptyBST) EmptyBST) (NodeBST 10 EmptyBST EmptyBST))

distance :: (Ord a) => a -> TreeBST a -> Int
distance _ EmptyBST = -1
distance x (NodeBST y l r)
  | x < y = 1 + distance x l
  | x > y = 1 + distance x r
  | otherwise = 0

includes :: (Ord a) => a -> TreeBST a -> Bool
includes _ EmptyBST = False
includes x (NodeBST y l r)
  | x < y = includes x l
  | x > y = includes x r
  | otherwise = True

parent :: (Ord a) => a -> a -> TreeBST a -> TreeBST a
parent x y t@(NodeBST z l r)
  | x == z = t
  | y == z = t
  | includes x l && includes y l = parent x y l
  | includes x r && includes y r = parent x y r
  | otherwise = t
