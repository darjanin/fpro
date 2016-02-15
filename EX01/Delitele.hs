module Delitele where
delitele :: Integer -> [Integer]
delitele n = vyskusajDelitele n [1..n]
  where vyskusajDelitele :: Integer -> [Integer] -> [Integer]
        vyskusajDelitele _ [] = []
        vyskusajDelitele n (x:xs) | mod n x == 0 = x : vyskusajDelitele n xs
                                  | otherwise    = vyskusajDelitele n xs

delitele' :: Integer -> [Integer]
delitele' n =  [ x | x <- [1..n],  mod n x == 0]
