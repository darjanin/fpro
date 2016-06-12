module Cvicenie2 where

delitele :: Int -> [Int]
delitele n = [d | d <- [1..n], mod n d == 0]

prvocislo :: Int -> Bool
prvocislo n = delitele n == [1, n]

prvocisla :: Int -> [Int]
prvocisla n = [p | p <- [1..n], prvocislo p]

prvocisla' :: [Int]
prvocisla' = 2 : [p | p <- [3..], odd p, prvocislo p]

rozdielSuctu :: [Integer] -> Integer
rozdielSuctu [] = 0
rozdielSuctu [x] = -x
rozdielSuctu (x:y:xs) = (y - x) + rozdielSuctu xs
