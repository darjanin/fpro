module Najcastejsi where
import Data.List

count x = length . filter (==x)

vyskyt xs = sort $ nub $ [(count x xs, x) | x <- xs]

najcastejsi :: [Int] -> Int
najcastejsi xs = snd $ head $ reverse $ vyskyt xs

najzriedkavejsi :: [Int] -> Int
najzriedkavejsi xs = snd $ head $ vyskyt xs

median :: [Int] -> Int
median xs = (sort xs)!!stred
  where
    stred = div (length xs) 2
