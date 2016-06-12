module SudokuStvorce where
import Data.List

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce m    = concat [ [ [ m!!(3*i+k)!!(3*j+l) | k <- [0..2], l <- [0..    2] ] | j <- [0..2] ] | i <- [0..2]]

sudokuStlpce :: [[Int]] -> [[Int]]
sudokuStlpce m = [[ m!!i!!j | i <- [0..8] ] | j <- [0..8]]

splna :: [[Int]] -> Bool
splna [] = True
splna (x:xs) = if x == nub x then splna xs else False

testSudokuStvorce :: [[Int]] -> Bool
testSudokuStvorce xs = riadky && stlpce && stvorce
  where
    riadky = splna xs
    stlpce = splna $ sudokuStlpce xs
    stvorce = splna $ sudokuStvorce xs
