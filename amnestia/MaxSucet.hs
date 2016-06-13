module MaxSucet where

-- DU 1 - 6k - 5. FPRO 01/Maximalny podzoznam

maxSucet :: [Int] -> (Int, [Int])
maxSucet = ms 0 [] 0 []

ms :: Int -> [Int] -> Int -> [Int] -> [Int] -> (Int, [Int])
ms maxsem ys maxtotal zs [] = (maxtotal, zs)
ms maxsem ys maxtotal zs (x:xs) = ms maxsem' ys' maxtotal' zs' xs
  where
    (maxsem', ys') = if maxsem + x > 0
      then (maxsem + x, ys ++ [x])
      else (0, [])
    (maxtotal', zs') = if maxtotal > maxsem'
      then (maxtotal, zs)
      else (maxsem', ys')
