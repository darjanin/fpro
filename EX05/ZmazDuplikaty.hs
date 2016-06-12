module ZmazDuplikaty where
zmazDuplikaty :: [Int] -> [Int]
zmazDuplikaty [] = []
zmazDuplikaty (x:xs) = (if elem x xs then [] else [x]) ++ zmazDuplikaty xs

-- zmazDuplikaty [1,2,1,2,1,3,1,2,3] vráti [1,2,3]
-- a = zmazDuplikaty [1,2,1,2,1,3,1,2,3]