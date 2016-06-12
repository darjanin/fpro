povsuvaj :: a -> [a] -> [[a]]
povsuvaj x [] = [[x]]
povsuvaj x ys = [(take i ys)++[x]++(drop (i) ys) | i <- [0..(length ys)]]
  where
    zlep x a b = a ++ [x] ++ b
