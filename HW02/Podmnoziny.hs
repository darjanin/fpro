module Podmnoziny where

podmnoziny :: [t] -> [[t]]
podmnoziny [] = [[]]
podmnoziny (x:xs) = tailSubsets ++ map (\a -> x:a) tailSubsets
  where
    tailSubsets = reverse $ podmnoziny(xs)


podmnozinyVPoradi :: [t] -> [[t]]
podmnozinyVPoradi xs = podmnoziny xs
