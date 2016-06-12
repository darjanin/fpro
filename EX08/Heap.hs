module Heap (Heap, emptyHeap, isEmptyHeap, rootHeap, insertIntoHeap, removeFromHeap, createHeap) where
-- min heap
data Heap a = EmptyHeap | NodeHeap a Int (Heap a) (Heap a) deriving (Show, Read)

rank :: Heap a -> Int
rank EmptyHeap = 0
rank (NodeHeap _ r _ _) = r

rootHeap :: (Ord a) => Heap a -> a
rootHeap EmptyHeap = error "Cannot return root of empty heap."
rootHeap (NodeHeap v _ _ _) = v

emptyHeap :: Heap a
emptyHeap = EmptyHeap

isEmptyHeap :: Heap a -> Bool
isEmptyHeap EmptyHeap = True
isEmptyHeap _         = False

createNode :: (Ord a) => a -> Heap a -> Heap a -> Heap a
createNode v l p
  | rank l >= rank p = NodeHeap v (rank p + 1) l p
  | otherwise        = NodeHeap v (rank l + 1) p l

zlucHeap :: (Ord a) => Heap a -> Heap a -> Heap a
zlucHeap h EmptyHeap = h
zlucHeap EmptyHeap h = h
zlucHeap h1@(NodeHeap x _ l1 p1) h2@(NodeHeap y _ l2 p2)
  | x <= y    = createNode x l1 (zlucHeap p h2)
  | otherwise = createNode y l2 (zlucHeap h1 p2)

insertIntoHeap :: (Ord a) => a -> Heap a -> Heap a
insertIntoHeap x h = zlucHeap h (createNode x EmptyHeap EmptyHeap)

removeFromHeap :: (Ord a) => Heap a -> Heap a
removeFromHeap EmptyHeap = error "Cannot remove element of empty heap."
removeFromHeap (NodeHeap _ _ l p) = zlucHeap l p
