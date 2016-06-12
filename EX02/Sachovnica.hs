module Sachovnica where

data  Sachovnica = Sachovnica [[Bool]] deriving(Eq)

instance Show Sachovnica where
  show (Sachovnica s) = concat [ '\n' : concat [if e then " #" else " ." | e <- row] | row <- s ]

veza   :: Int -> Int -> Sachovnica
veza x y = Sachovnica [ [ ( x == i || j == y ) && (x,y)/=(i,j) | j<-[0..7] ] | i<-[0..7]]

strelec   :: Int -> Int -> Sachovnica
strelec x y = Sachovnica [ [ abs(x-i) == abs(y-j) && (x,y)/=(i,j) | j<-[0..7] ] | i<-[0..7]]

kral   :: Int -> Int -> Sachovnica
kral x y = Sachovnica [ [ abs(x-i) <= 1 && abs(y-j) <= 1 && (x,y)/=(i,j) | j<-[0..7] ] | i<-[0..7]]

kon   :: Int -> Int -> Sachovnica
kon x y = Sachovnica [ [ (abs(x-i) == 2 && abs(y-j) == 1) || (abs(x-i) == 1 && abs(y-j) == 2)  | j <- [0..7] ] | i <- [0..7]]


-- Boys testy nepridalo body, preto pridavam aj tuto nech mam tie body.
dama   :: Int -> Int -> Sachovnica
dama x y = Sachovnica [ [ (( x == i || j == y ) && (x,y)/=(i,j)) || (abs(x-i) == abs(y-j) && (x,y)/=(i,j)) | j<-[0..7] ] | i<-[0..7]]
