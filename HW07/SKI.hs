module SKI where

type Var = String
--data Ski = S | K | I | APL Ski Ski deriving (Show)
--data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp
data LExpSki = APP LExpSki LExpSki
  | LAMBDA Var LExpSki
  | ID Var
  | S
  | K
  | I deriving (Eq)

instance Show LExpSki
  where
    --show :: LExp -> String
    show (LAMBDA v e) = '\\' : v ++ " -> " ++ show e
    show (ID v) = v
    show S = "S"
    show I = "I"
    show K = "K"
    show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

-- pouzite pravidla z http://goodmath.blogspot.sk/2006/05/from-lambda-calculus-to-combinator.html
-- S = \xyz.(xz)(yz)
-- K = \xy.x
-- I = \x.x
toSki :: LExpSki -> LExpSki
toSki (APP e1 e2) = APP (toSki e1) (toSki e2)
toSki (LAMBDA x (ID y))
  | x == y = I
toSki (LAMBDA x e)
  | not (x `elem` (free e)) = APP K (toSki e)
toSki (LAMBDA x (LAMBDA y e))
  | x `elem` (free e) = toSki (LAMBDA x (toSki (LAMBDA y e)))
toSki (LAMBDA x (APP e1 e2)) = APP (APP S (toSki (LAMBDA x e1))) (toSki (LAMBDA x e2))
toSki x = x

free :: LExpSki -> [Var]
free (ID v) = [v]
free (LAMBDA v e) = [x | x <- free e, x /= v]
free (APP e1 e2) = free e1 ++ free e2
free _ = []

reduceSki :: LExpSki -> LExpSki
reduceSki e = if newE == e then e else reduceSki newE
  where
    newE = reduceStep e

reduceStep :: LExpSki -> LExpSki
reduceStep (APP (APP (APP S f) g) x) = APP (APP f x) (APP g x)
reduceStep (APP (APP K c) x) = c
reduceStep (APP K c) = c
reduceStep (APP I x) = x
reduceStep (APP S f) = APP S (reduceStep f)
reduceStep (APP (APP S f) g) = APP (APP S (reduceStep f)) (reduceStep g)
reduceStep (APP e1 e2) = reduceStep (APP (reduceStep e1) (reduceStep e2))
reduceStep e = e

--APP (APP S (APP S I)) (APP (APP S K) I)
--S (S I) (S K I)

--APP (APP S (APP K (APP S I))) (APP (APP S (APP K K)) I)
--(S (K (S I))) ((S (K K)) I)
--(S (S I)) (S K I)
c = (LAMBDA "f" (LAMBDA "x" (LAMBDA "y" (APP (APP (ID "f") (ID "y")) (ID "x")))))
a1 = (LAMBDA "f" (LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "f") (APP (APP (ID "f") (ID "x")) (ID "y"))) (ID "z"))))))

toLExp :: LExpSki -> LExpSki
toLExp I = LAMBDA "x" (ID "x")
toLExp K = LAMBDA "x" (LAMBDA "y" (ID "x"))
toLExp S = LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "z")) (APP (ID "y") (ID "z")))))
toLExp (APP e1 e2) = APP (toLExp e1) (toLExp e2)
