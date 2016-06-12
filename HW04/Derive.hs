module Derive where
data Exp = ICon Int
  | Var String
  | Add Exp Exp
  | Sub Exp Exp
  | UMin Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pwr Exp Exp
  | Ln Exp
      deriving (Eq, Ord, Read, Show)

derive :: Exp -> String -> Exp
derive (ICon _) _ = ICon 0
derive (Var x) y | x == y = ICon 1
                        | otherwise = ICon 0
derive (Add e1 e2) x = urobAdd (derive e1 x) (derive e2 x)
derive (Sub e1 e2) x = urobSub (derive e1 x) (derive e2 x)
derive (Mul e1 e2) x = urobAdd (urobMul (derive e1 x) e2) (urobMul e1 (derive e2 x))
derive (Div e1 e2) x = urobDiv (urobSub (urobMul (derive e1 x) e2) (urobMul e1 (derive e2 x))) (Pwr e2 (ICon 2))
--  x^x(x'*lnx +x(lnx)')
--derive (Pwr e1 e2) x = urobMul (urobPwr e1 e2) (urobAdd (urobMul (derive e1 x) urobLn e1) (urobMul e2))
derive (UMin e) x = UMin (derive e x)
derive (Ln e) x = urobMul (urobDiv (ICon 1) e) (derive e x)

urobAdd :: Exp -> Exp -> Exp
urobAdd (ICon x) (ICon y) = ICon (x+y)
urobAdd e1 e2
  | e1 == (ICon 0) = e2
  | e2 == (ICon 0) = e1
  | e1 == e2       = Mul (ICon 2) e1
  | otherwise = Add e1 e2

urobSub :: Exp -> Exp -> Exp
urobSub (ICon x) (ICon y) = ICon (x-y)
urobSub e1 e2
  | e1 == (ICon 0) = UMin e2
  | e2 == (ICon 0) = e1
  | e1 == e2 = ICon 0
  | otherwise = Sub e1 e2

urobMul :: Exp -> Exp -> Exp
urobMul (ICon x) (ICon y) = ICon (x*y)
urobMul e1 e2
  | e1 == (ICon 0) = ICon 0
  | e2 == (ICon 0) = ICon 0
  | e1 == (ICon 1) = e2
  | e2 == (ICon 1) = e1
  | otherwise = Mul e1 e2

urobDiv :: Exp -> Exp -> Exp
urobDiv (ICon x) (ICon y) = ICon (div x y)
urobDiv e1 e2
  | e1 == (ICon 0) = ICon 0
  | e2 == (ICon 1) = e1
  | otherwise = Div e1 e2

urobPwr :: Exp -> Exp -> Exp
urobPwr (ICon x) (ICon y) = ICon (x^y)
urobPwr e1 e2
  | e1 == (ICon 0) = ICon 0
  | e2 == (ICon 0) = ICon 1
  | otherwise = Pwr e1 e2

urobLn :: Exp -> Exp
urobLn e = Ln e


-----------------
expToStr :: Exp -> String
expToStr (ICon x) = show x
expToStr (Var x) = x
--expToStr (Add e1 e2) = "(" ++ expToStr e1 ++ " + " ++ expToStr e2 ++ ")"
--expToStr (Sub e1 e2) = "(" ++ expToStr e1 ++ " - " ++ expToStr e2 ++ ")"
expToStr (Add e1 e2) = expToStr e1 ++ " + " ++ expToStr e2
expToStr (Sub e1 e2) = expToStr e1 ++ " - (" ++ expToStr e2 ++ ")"
expToStr (Mul e1 e2) = expToStr e1 ++ " * " ++ expToStr e2
expToStr (Div e1 e2) = "(" ++ expToStr e1 ++ ") / (" ++ expToStr e2 ++ ")"
expToStr (Pwr e1 e2) = "((" ++ expToStr e1 ++ ") ^ (" ++ expToStr e2 ++ "))"
expToStr (UMin e) = "-(" ++ expToStr e ++ ")"
expToStr (Ln e) = "ln (" ++ expToStr e ++ ")"

d v p = putStrLn (expToStr (derive v p))

--e4 = ICon 2
--e2 = Var "x"
--e3 = Add e4 e2

--Doplnte modul o derivovanie súčinu a podielu.
--a funkcie
--urobMul, urobDiv, urobPwr :: Exp -> Exp -> Exp
--ktoré z dvoch výrazov vytvoria súčin, podiel, resp. umocnenie a pri tom výraz zjednodušia a
--urobLn :: Exp -> Exp
--ktoré z jedného logaritmus, a pritom výraz zjednoduší.
--V prípade potreby si definujte ešte aj funkciu zjednodus :: Exp -> Exp, ktorá zjednoduší výraz
--Usilujte sa aby zderivované výrazy boli čo najjednoduchšie. Jednoduchosť výrazu nie je dobre definovaná,takže sa riaďte sojím úsudkom- usilujte sa aby program vytvoril výraz aký by ste vytvorili vy. Iné kritérium,je napríklad čo najkratší výraz.
--Doplňte aj funkciu expToStr.
--K vašemu riešeniu priložte na koniec do komentara {-  ... -}
--testy (vstup, výstup), na ktorých ste testovali (vrátane troch nižšie uvedených)

--Napríklad
vx = Var "x"
e1 = Pwr vx vx
e2 = Mul (Add (ICon 2) vx) vx

-- d (Div (Add (Var "x") (ICon 2)) (Var "x")) "x" moze vratit (x - (x + 2)) / ((x) ^ (2))
--d e1 "x" môže vrátit (x^x)*(Ln(x) + 1)
--d (derive e11 "x") "x" môže vrátiť  (x^x)*((Ln(x) + 1)^2) + x^(x + -1)
--d (derive (derive e11 "x") "x") "x" môže vrátiť  (x^x)*((Ln(x) + 1)^3) + 2*((x^(x + -1))*(Ln(x) + 1)) + (x^(x + -1))*(Ln(x) + (x + -1)/x)