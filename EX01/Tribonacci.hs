module Tribonacci where
trib :: Integer -> Integer
trib 0 = 1
trib 1 = 1
trib 2 = 1
trib n = trib (n - 3) + trib (n - 2) + trib (n - 1)
