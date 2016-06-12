-- kod nie je napisany aby sa dal spustit priamo v ghci, kedze ma byt rucne kontrolovany postup
-- v editore sa to prijemnejsie pise a kontroluje uzatvorkovanie

-- reverse (xs ++ ys) = reverse ys ++ reverse xs
-- ak xs = [] a ys = [] tak plati
reverse ([] ++ []) == reverse [] ++ reverse [] == []

reverse (xs ++ ys)
reverse ([x1,x2,..,xn] ++ [y1,y2,..,yn])
reverse [x1,x2,..,xn,y1,y2,..,yn]
[yn,yn-1,..,y1,xn,xn-1,..,x1] -- k comu sa chcem dopracovat v druhej variante

reverse ys ++ reverse xs
reverse [y1,y2,..,yn] ++ reverse [x1,x2,..,xn]
[yn,yn-1,..,y1] ++ [xn,xn-1,..,x1]
[yn,yn-1,..,y1,xn,xn-1,..,x1] -- dopracoval som sa k rovnakemu vyslekdu

-- rovnost plati

-- foldr :: (a - > b -> b) -> b -> [a] -> b
-- foldr f a [] = a
-- foldr f a (x:xs) = f x (foldr f a xs)
-- foldr f a (xs ++ ys) = foldr f (foldr f a ys) xs
foldr f a ([] ++ []) == a
foldr f (foldr f a []) [] == a

foldr f a (xs ++ ys)
foldr f a ([x1,x2,..,xn] ++ [y1,y2,..,yn])
foldr f a ([x1,x2,..,xn,y1,y2,..,yn])
(f x1 (f x2 (..(f xn (f y1 (f y2 (..(f yn a)))))))) -- vysledok ktory chceme dostat aj druhym postupom

foldr f (foldr f a ys) xs
foldr f (foldr f a [y1,y2,..,yn]) [x1,x2,..,xn]
A = foldr f a [y1,y2,..,yn] == (f y1 (f y2 (..(f a yn))))
foldr f A [x1, x2,..,xn]
(f x1 (f x2 (..(f xn A))))
(f x1 (f x2 (..(f xn (f y1 (f y2 (..(f a yn)))))))) -- podarilo sa dostat k rovnakemu vysledku
