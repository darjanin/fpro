-- derivovac

data Exp = ICon Int
           | Var String
           | Add Exp Exp
           | Sub Exp Exp
           | UMin Exp
          deriving (Eq, Ord, Read, Show)

derive :: Exp -> String -> Exp
derive (ICon _) _ = ICon 0
derive (Var x) y | x == y    = ICon 1
                 | otherwise = ICon 0
derive (Add e1 e2) x = urobAdd (derive e1 x) (derive e2 x)
derive (Sub e1 e2) x = Sub (derive e1 x) (derive e2 x)

urobAdd :: Exp -> Exp -> Exp
urobAdd (ICon x) (ICon y) = ICon (x + y)
urobAdd (ICon 0) e = e
urobAdd e (ICon 0) = e
urobAdd e1 e2 = Add e1 e2

expToStr :: Exp -> String
expToStr (ICon x) = show x
expToStr (Var x) = x
expToStr (Add e1 e2) = expToStr e1 ++ " + " ++ expToStr e2
expToStr (Sub e1 e2) = expToStr e1 ++ " - " ++ "(" ++ expToStr e2 ++ ")"
expToStr (UMin e) = "-(" ++ expToStr e ++ ")"

d v p = putStrLn (expToStr (derive v p))

e1 = ICon 2
e2 = ICon 3
vx = Var "x"
vy = Var "y"
e3 = Add e1 e2
e4 = Add e2 vx
e5 = Add e1 vy
e6 = Add e3 e4
e7 = Add e4 e3
e9 = Sub e3 e6
