module KSucin where

cart :: [[t]] -> [[t]]
cart (x:y:[]) = spoj x y
cart (x:xs) = spojSN x (cart xs)
--

-- spojN :: [[t]] -> [[t]] -> [[t]]
-- spojN x (y:ys) = spoj x y ++ spojN xs ys
--
spojSN :: [t] -> [[t]] -> [[t]]
spojSN x [y] = spoj x y
spojSN x (y:ys) = spoj x y ++ spojSN x ys

spoj :: [t] -> [t] -> [[t]]
spoj [x] ys = [x:ys]
spoj (x:xs) ys = [x:ys] ++ spoj xs ys
--
sparuj :: t -> [t] -> [t]
sparuj a xs = a:xs
