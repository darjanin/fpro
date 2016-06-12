module Sqrt where

odmocnina :: Float -> Float -> Float
odmocnina x eps = newton x x eps

-- definition from https://cs.wikipedia.org/wiki/Metoda_tečen

newton :: Float -> Float -> Float -> Float
newton a x eps = if diff < eps then x else newton a newX eps
  where
    newX = 0.5 * (x + a / x)
    diff = abs (x - newX)
