module TypyJeUplny where
  
data Tree a = EmptyT | NodeT a (Tree a)(Tree a)
   deriving (Show,Read,Eq)
