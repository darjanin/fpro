module DropIndex where

drop'       :: Int -> [t] -> [t]
drop' n xs  =  reverse ((foldr pom (\_ -> []) (reverse xs)) n) where
                pom x h = \n -> if n == 0 then []
                                 else (h (n-1))

(!!!!)      :: [t] -> Int -> t
xs !!!! n   =  last ((foldr pom (\_ -> []) xs) (n+1)) where
                pom x h = \n -> if n == 0 then []
                                 else x:(h (n-1))

-- naprogramujme take pomocou foldl/r
take' :: Int -> [a] -> [a]
take' n xs  =  (foldr pom (\_ -> []) xs) n where
                pom x h = \n -> if n == 0 then []
                                 else x:(h (n-1))

take'' :: Int -> [a] -> [a]
take'' n xs  =  (foldr pom (\_ -> []) xs) n where
                  pom x h n = if n == 0 then []
                              else x:(h (n-1))

take''' n xs = foldr (\a h -> \n -> case n of
                                         0 -> []
                                         n -> a:(h (n-1)) )
                     (\_ -> [])
               xs
               n