module Luhn where

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits(div n 10) ++ (mod n 10):[]

doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond [x] = x:[]
doubleSecond (x:y:zs) = x:(y * 2):doubleSecond zs

cardnumber :: Integer -> Bool
cardnumber n = 0 == (mod (sum $ [ if x > 9 then sum $ digits x else x | x <- doubleSecond $ reverse $ digits n]) 10) 
