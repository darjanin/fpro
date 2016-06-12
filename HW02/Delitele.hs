module Delitele where

disjunktne :: [Int] -> [Int] -> Bool
disjunktne [] _ = True
disjunktne (x:xs) ys = if elem x ys then False else disjunktne xs ys

nesudelitelne :: Int -> Int -> Bool
nesudelitelne a b = disjunktne (delitele a) (delitele b)

prvocislo :: Int -> Bool
prvocislo 0 = False
prvocislo 1 = False
prvocislo n = delitele n == []

delitele :: Int -> [Int]
delitele n = vyskusajDelitele n [2..(n-1)]
  where vyskusajDelitele :: Int -> [Int] -> [Int]
        vyskusajDelitele _ [] = []
        vyskusajDelitele n (x:xs)
          | mod n x == 0 = x : vyskusajDelitele n xs
          | otherwise    = vyskusajDelitele n xs
