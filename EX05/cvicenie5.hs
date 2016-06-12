--cvicenia 5
-----------
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

e1 = ICon 10
e2 = Var "x"
e3 = Add e1 e2
e4 = Sub e3 e3

-- pocet premennych vo vyraze, ich zoznam
zoznamVar :: Exp -> [Exp]
zoznamVar e = zmazDuplikaty $ zoznamVar' [] e

zoznamVar' :: [Exp] -> Exp -> [Exp]
zoznamVar' z (ICon _) = z
zoznamVar' z (Var v) = (Var v):z
zoznamVar' z (Add e1 e2) = zoznamVar' z e1 ++ zoznamVar' z e2
zoznamVar' z (Sub e1 e2) = zoznamVar' z e1 ++ zoznamVar' z e2

zmazDuplikaty [] = []
zmazDuplikaty (x:xs) = (if elem x xs then [] else [x]) ++ zmazDuplikaty xs

-- dosadenie konstanty za premenne / vyhodnotenie vyrazu - derivacia v bode
-- dosad CO za PREMENNU a vrat VYRAZ
dosad :: Exp -> String -> Exp -> Exp
dosad co s (Var v) = if v == s then co else (Var v)
dosad co s (ICon c) = ICon c
dosad co s (Add e1 e2) = Add (dosad co s e1) (dosad co s e2)
dosad co s (Sub e1 e2) = Sub (dosad co s e1) (dosad co s e2)

e5 = dosad e1 "x" (dosad e3 "x" e4)
-- pocet binarnych/unarnych funkcii vo vyraze (ich zoznamy)
-- "hlbka" vyrazu
hlbka :: Exp -> Int
hlbka (Add e1 e2) = 1 + max (hlbka e1) (hlbka e2)
hlbka (Sub e1 e2) = 1 + max (hlbka e1) (hlbka e2)
hlbka _ = 1

-- "sirka" vyrazu
--sirka :: Exp -> Int
--sirka

-- aky problem by bol so substituciou za premennu? bol by nejaky? kedy by bol?
-- modifikacia na vseobecny strom Strom a = Empty | Branch a [Strom]
-- ako sa zmeni sirka hlbka?
-- preorder, inorder, postorder
-- aky hlboky zasobnik potrebujeme na vyhodnotenie vyrazu?
-- mobil na du sirka, ci sa nezamota,...

-- pravidla derivovania
-- x^x = e^(ln x)^x = e^(x * ln x)
-- (x^x)' = (e^(x * lnx)) =
--   = e^(x*lnx) * (x * lnx)' = x^x(x'*lnx +x(lnx)')