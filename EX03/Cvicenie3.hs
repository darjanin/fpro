module Cvicenie3 where

-- foldl1 f [a1,...,an] = (... (f (f a1 a2) a3 ... an)
-- foldl1 f [a1] = a1
-- foldl1 f [] = undefined
-- priklad
-- foldl1 (/) [64,4,2,8] = 1

---------------------- definujte foldl1 pomocou foldl1

-- foldr1 f [a1,...,an] = (f a1 (f a2 ... (f a_n-1 an)))
-- foldr1 f [a1] = a1
-- foldr1 f [] = undefined
-- priklad
-- foldr1 (/) [8,12,24,4] = 4.0

------------------------- definujte foldr pomocou foldr1
 
-- scanr/scanl

 -- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
 -- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 

 ------------------------ definujte scanl pomocou foldl

 ------------------------ definujte scanl pomocou foldl efektivnejsie (napr. pomocou reverse)

 ------------------------ definujte foldl pomocou scanl

 ------------------------ definujte scanr pomocou foldr

 ------------------------ definujte foldr pomocou scanr
 ------------------------ definujte map f pomocou foldr
 ------------------------ definujte concat pomocou foldr
 ------------------------ definujte list-comprehension [f x | x<-xs] pomocou foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs


myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f a [] = a
myFoldr f a (x:xs) = f x (myFoldr f a xs)


