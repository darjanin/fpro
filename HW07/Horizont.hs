module Horizont where
import Data.List
horizont :: [Int] -> [Int]
horizont [] = [0]
horizont xs = concat $ map (\x -> [fst x, snd x]) $ sort . removeDuplicates . generateCoordinates $ xs

horizont' :: [Int] -> [(Int, Int)]
horizont' = sort . removeDuplicates . generateCoordinates

generateCoordinates [] = []
generateCoordinates (x:h:w:xs) = [(x,h), (x+w,h)] ++ generateCoordinates xs

removeDuplicates [] = []
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs else x:(removeDuplicates xs)

a :: [Int]
a = [3,3,2,1,1,7,2,2,2]
