module Int2EnglishString where

convert :: Int -> String
convert n
  | n < 10 = ones !! n
  | n < 20 = teens !! (mod n 10)
  | n < 100 = (tens !! ((div n 10) - 2)) ++ if (mod n 10) > 0 then "-" ++ convert (mod n 10) else []
  | n < 1000 = (convert (div n 100)) ++ " hundred and " ++ convert (mod n 100)
  | otherwise = (convert (div n 1000)) ++ " thousand" ++ if (mod n 1000) > 0 then (if (mod n 1000) < 99 then " and " else " ") ++ convert (mod n 1000) else []
    where
      ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
      tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
