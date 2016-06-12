module JeUplny where
import TypyJeUplny

-- data Tree a = EmptyT | NodeT a (Tree a)(Tree a)
--    deriving (Show,Read,Eq)

jeUplny :: (Eq a) => Tree a -> Bool
jeUplny EmptyT = True
jeUplny (NodeT _ l r)
  | minHeight r < maxHeight l = False
  | l /= EmptyT && r /= EmptyT = jeUplny l && jeUplny r
  | l /= EmptyT && r == EmptyT = jeUplny l
  | l == EmptyT && r /= EmptyT = False
  | l == EmptyT && r == EmptyT = True

minHeight :: (Eq a) => Tree a -> Int
minHeight EmptyT = 0
minHeight (NodeT _ l r)
  | l == EmptyT || r == EmptyT = 1
  | otherwise = min (1 + (minHeight l)) (1 + (minHeight r))

maxHeight :: (Eq a) => Tree a -> Int
maxHeight EmptyT = 0
maxHeight (NodeT _ l r)
  | l == EmptyT && r == EmptyT = 1
  | otherwise = max (1 + (maxHeight l)) (1 + (maxHeight r))


t = NodeT 5 (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 1 EmptyT EmptyT)) (NodeT 1 EmptyT EmptyT)) (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 1 EmptyT EmptyT)) (NodeT 1 EmptyT EmptyT))
t2 = NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 1 EmptyT EmptyT)) (NodeT 1 EmptyT EmptyT)
