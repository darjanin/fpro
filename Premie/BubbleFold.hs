module BubbleFold where

bubbleFold :: Ord t => [t] -> [t]
bubbleFold xs = foldl (\(a:as) _ -> foldl (\acc b -> if last acc < b then acc ++ [b] else [b] ++ acc) [a] as) xs xs
