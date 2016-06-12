-- kodovanie UTF-8 bez BOM (Notepad++)
module Beta where

import Terms
instance Show LExp
  where
    --show :: LExp -> String
    show (LAMBDA v e) = '\\' : v ++ " -> " ++ show e
    show (ID v) = v
    show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

---- `subterms l` vráti zoznam všetkých podtermov termu `l`
subterms :: LExp -> [LExp]
subterms (ID v) = [ID v]
subterms (LAMBDA v e) = [LAMBDA v e] ++ subterms e
subterms (APP e1 e2) = [APP e1 e2] ++ subterms e1 ++ subterms e2

a = APP (LAMBDA "x" (ID "x")) (LAMBDA "y" (ID "y"))

---- `free l` vráti zoznam všetkých premenných, ktoré majú voľný výskyt v terme `l`
free :: LExp -> [Var]
free (ID v) = [v]
free (LAMBDA v e) = [x | x <- free e, x /= v]
free (APP e1 e2) = free e1 ++ free e2

---- `bound l` vráti zoznam všetkých premenných, ktoré majú viazaný výskyt v terme `l`
bound :: LExp -> [Var]
bound (ID v) = []
bound (LAMBDA v e) = bound(e) ++ [v]
bound (APP e1 e2) = bound e1 ++ bound e2

l1 = ID "x"
l2 = ID "y"
---- `substitute v k l` substituuje všetky voľné výskyty `v` v terme `l` za `k`
substitute :: Var -> LExp -> LExp -> LExp
substitute v k (ID v1) = if v == v1 then k else ID v1
substitute v k (APP e1 e2) = APP (substitute v e1 k) (substitute v e2 k)
substitute x k (LAMBDA y e)
  | x == y = LAMBDA y e
  -- nove mena premennych budu x0, x1, x2 ... take, co nie su volne v e, /= x, /= y
  | (x `elem` free(e)) && (y `elem` free(k)) = LAMBDA w (substitute x k (substitute y (ID w) e))
  | otherwise = LAMBDA y (substitute x k e)
    where
      w = head [ nova | nova <- map (('x':).show) [0..], nova `notElem`  (free e), nova /= x, nova /=y]