-- [x | x<-xs] = xs
-- [f x | x <- xs] = map f xs
-- [e | x <- xs, p x,...] = [e | x <- filter p xs, ...]
-- [e | x <- xs, y <- ys, ...] = concat[[e | y <- ys, ...] | x <- xs]

-- a) [x|xs<-xss, x<-xs, odd x]
concat $ map (filter odd) xss
-- b) [(x,y)|x<-xs,p x, y<-ys]
concat $ map (\x -> map (\y -> (x, y)) ys) (filter p xs)
