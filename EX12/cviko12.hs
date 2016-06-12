-- cvicenie 11
import Prelude hiding ((<*>), (<|>))
import Data.Char
import Data.List

type Parser symbol result = [symbol] -> [([symbol],result)]

symbola :: Parser Char Char
symbola [] = [] -- ak nie je nič na vstupe
symbola (x:xs) | x=='a' = [ (xs, 'a') ] -- ak je 'a' na vstupe
               | otherwise= [] -- ak nie je 'a' na vstupe

-- to iste ale parametrizovane
symbol :: Eq s => s -> Parser s s
symbol a [] = []
symbol a (x:xs) | a==x = [ (xs, x) ]
                | otherwise= []

-- inak zapisane
symbol' :: Eq s => s -> Parser s s
symbol' a [] = []
symbol' a (x:xs) = [ (xs, a) | a == x ]

-- odreze prefix ak je rovnaky
token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [ (drop n xs, k)]
           | otherwise = []
   where n = length k

k0 = []
k1 = ["kuk"]
k2 = ["ahoj", "kuk"]
k3 = ["ahoj"]

k4 = "kuk"
k5 = "ku"
k6 = "u"

--vseobecnejsi symbol,
satisfy :: (s -> Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) = [ (xs, x) | p x ]

--symbol inak, cv. 1.
symbol'' a = satisfy (\x->x==a)
symbol''' a = satisfy (==a)

--cv. 2, a 3.
digit10 :: Parser Char Char
digit10 = satisfy isDigit

hexa :: Parser Char Char
hexa = satisfy (\x->elem x "0123456789ABCDEF")

hexa' = satisfy (\x-> isDigit x || elem x ['A'..'F'])
hexa'' =satisfy isHexDigit

epsilon :: Parser s () -- () je ako typ void
epsilon xs = [ ( xs, () ) ] -- () hodnota typu (), ako null

fail :: Parser s r
fail xs = []

succeed :: r -> Parser s r
succeed v xs = [ (xs, v) ]

infixr 6 <*> -- sekvenčné zreťazenie analyzátorov
infixr 4 <|>

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs = [ (xs2, (v1,v2))
                    | (xs1, v1) <- p1 xs,
                      (xs2, v2) <- p2 xs1
                 ]
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

-- cv.5
yes = token "YES"
no = token "NO"
yesno = yes <|> no

-- toto asi nie, kvoli typom
-- no' = symbol 'N' <*> symbol 'O' -- <*> succeed "NO"
-- yes' = symbol 'Y' <*> symbol 'E' <*> symbol 'S' -- <*> succeed "YES"
-- yesno' = yes' <|> no'

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (==' ')

just :: Parser s a -> Parser s a
just p = filter (null.fst) . p

-- cv 6
just' p xs = [x | x <- p xs, null $ fst x]

infix 5 <@
(<@) :: Parser s a -> (a -> b) -> Parser s b
(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]

list :: (a, [a]) -> [a]
list (x, xs) = (x:xs)

infixr 6 <:*>
(<:*>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q = p <*> q <@ list

-- cv 7
infixr 6 <:**>
(<:**>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:**> q = p <*> q <@ (\(x, xs) -> (x:xs))

many :: Parser s a -> Parser s [a]
many p = p <:*> (many p) <|> succeed []

single x = []

many1 :: Parser s a -> Parser s [a]
many1 p = p <:*> many p
identifier = many1 (satisfy isAlpha)

option :: Parser s a -> Parser s [a]
option p = p <@ single <|> succeed []

one :: Parser s a -> Parser s [a]
one p = p <@ single

sequence' :: [Parser s a] -> Parser s [a]
sequence' = foldr (<:*>) (succeed [])

-- cv 8
sequence'' [] = succeed []
sequence'' (x:xs) = x <:*> (sequence'' xs)

-- cv 8'
token' :: Eq s => [s] -> Parser s [s]
token' = sequence' . map symbol

-- cv 9
telNumber = sequence' ([symbol '0', symbol '9'] ++ replicate 8 digit10)
