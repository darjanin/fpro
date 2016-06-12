module TypyTree where

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Read, Show)
