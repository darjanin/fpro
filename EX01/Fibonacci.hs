module Fibonacii where
fib' :: Integer -> Integer
fib' n = fibAc 0 1 n
  where fibAc :: Integer -> Integer -> Integer -> Integer
        fibAC a _ 0 = a
        fibAc a b n = fibAc b (a+b) (n-1)
