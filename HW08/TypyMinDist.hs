module TypyMinDist where

data TreeBST a = EmptyBST | NodeBST a (TreeBST a) (TreeBST a)
   deriving (Show, Read)
