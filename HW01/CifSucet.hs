module CifSucet where

-- http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Integer]
digits 0 = []
digits n = digits(div n 10) ++ (mod n 10):[]

cifSucet :: [Integer] -> Integer
cifSucet [] = 0
cifSucet [x] = x
cifSucet xs = cifSucet $ digits $ sum xs

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b
  | (cifSucet $ digits a) == 5 = count $ toInteger $ length [a..b]
  | otherwise = jCislaPocet (a + 1) b

count :: Integer -> Integer
count n
  | mod n 9 == 0 = div n 9
  | otherwise    = (div n 9) + 1
