module Cislo where

g :: Integer -> Integer -> Integer
g 0 m = m
g n m
  | odd n = g (n `div` 2) (2 * m + 1)
  | even n = g (n `div` 2) (2 * m)

f :: Integer -> Integer
f n = g n 0

-- Napíšte do modulu Cislo funkciu cislo n x, ktorá vypočíta najmenšie y>n také, že f(y)=x, ak také y neexistuje výsledok je 0

cislo :: Integer -> Integer -> Integer
cislo n x
  | n == (2 * x) = 0
  | (mod n 10 == 0) && (mod x 10 == 0) = -2
  | f(n) == x = n
  | otherwise = cislo (n+1) x