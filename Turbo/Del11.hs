module Del11 where
delitelne11 :: Integer -> Bool
delitelne11 0 = True


sumParne :: String -> Integer -> Integer 
sumParne [] value = value
sumParne (_:y:xs) value = sumParne xs (value + read (y:[])) 

