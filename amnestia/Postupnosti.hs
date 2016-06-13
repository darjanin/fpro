module Postupnosti where

-- DU 3 - 6k - 5. Najdlhšia súvislá rastúca postupnosť

nsrp :: (Ord t) => [t] -> Int
nsrp [] = -1
nsrp (x:xs) = maxlen
  where
    (maxlen, _, _) = foldl f (1, 1, x) xs
    f (maxlen, len, previous) x =
      if x > previous then (max maxlen (len+1), len+1, x) else (max maxlen 1, 1, x)
