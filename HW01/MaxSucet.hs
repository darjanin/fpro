module MaxSucet where

maxSucet :: [Int] -> (Int, [Int])
maxSucet [] = (0, [])
maxSucet (x:xs) = maxSucet xs
