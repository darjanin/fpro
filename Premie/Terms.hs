module Terms where
--import Data.List.Split
-- identifikator premennej je String
type Var = String
-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp deriving(Eq)

instance Show LExp
  where
    --show :: LExp -> String
    show (ID x) = x
    show (APP x y) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"
    show (LAMBDA x y) = "?" ++ x ++ "." ++ (show y)

fromString :: String -> LExp
fromString s@(x:xs)
  | x == '(' = APP (fromString $ fst appSplit) (fromString $ snd appSplit)
  | x == '?' = LAMBDA (fst lambdaSplit) (fromString $ snd lambdaSplit)
  | otherwise = ID (s ++ "'")
  where
    appSplit = splitOn ' ' (init xs)
    lambdaSplit = splitOn '.' xs

-- studium uloh z githubu tohto predmetu som nasiel toto riesenie splitOn a lepsie mi nenapadlo
-- pokial to nemozem pouzit pri rieseni tejto premievoje ulohy tak mi nedajte body za ulohu
-- nesnazim sa podvadzat alebo porusit pravidla predmetu iba pouzivam materialy k predmetu
splitOn :: Char -> String -> (String, String)
splitOn c xs = splitOn' [] xs c 0
  where
    splitOn' xs [] c n    = (reverse xs,[])
    splitOn' xs (y:ys) c n
      | y==c && n==0 = (reverse xs, ys)
      | y=='('       = splitOn' (y:xs) ys c (n+1)
      | y==')'       = splitOn' (y:xs) ys c (n-1)
      | otherwise    = splitOn' (y:xs) ys c n


--omega = "?x.(x x)"
-- omegaLExp = LAMBDA ID "x" APP ID "x" ID "x"
--omega = fromString "?x.(x x)"
--succ  = fromString "?n.?f.?x.(f ((n f) x))"
--plus =  fromString "?m.?n.?f.?x.((m f) ((n f) x))"
--times = fromString "?m.?n.?f.?x.((m (n f)) x)"
--power = fromString "?m.?n.(n m)"
--true =  fromString "?x.?y.x"
--false = fromString "?x.?y.y"
--land =  fromString "?x.?y.((x y) FALSE)"
--lor =   fromString "?x.?y.((x TRUE) y)"
--lnot =  fromString "?x.((x FALSE) TRUE)"
--lxor =  fromString "?x.?y.((x ((y FALSE) TRUE)) ((y TRUE) FALSE))"
--y =     fromString "?f.(?x.(f (x x)) ?x.(f (x x)))"
--isZero =fromString "?n.((n ?y.FALSE) TRUE)"
--pair =  fromString "?x.?y.?c.((c x) y)"
--first = fromString "?x.(x TRUE)"
--second =fromString "?x.(x FALSE)"
