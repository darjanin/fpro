module RozdielSuctu where

rozdielSuctu :: [Integer] -> Integer
rozdielSuctu [] = 0
rozdielSuctu [x] = -x
rozdielSuctu (x:y:zs) = (y - x) + rozdielSuctu zs

rozdielSuctu''' xs = p - n
  where (p,n) = foldr (\x -> \(a,b) -> (b,a+x)) (0,0) xs
