module BVS (BVS, prazdnyBVS, jeVBVS, pridajDoBVS, vytvorBVS, zmazZBVS, inorder) where

data BVS a = PrazdnyBVS | VrcholBVS a (BVS a) (BVS a) deriving (Show, Read)

prazdnyBVS :: BVS a
prazdnyBVS = PrazdnyBVS

jeVBVS :: (Ord a)  => a -> BVS a -> Bool
jeVBVS _ PrazdnyBVS = False
jeVBVS x (VrcholBVS y l r)
  | x == y    = True
  | x < y     = jeVBVS x l
  | otherwise = jeVBVS x r

vytvorBVS :: (Ord a) => [a] -> BVS a
vytvorBVS xs = foldr pridajDoBVS PrazdnyBVS xs

-- pre utriedene iba
vytvorBVS' :: (Ord a) => [a] -> BVS a
vytvorBVS' [] = PrazdnyBVS
vytvorBVS' xs = VrcholBVS h (vytvorBVS' l) (vytvorBVS' p)
  where
      l = take d xs
      (h:p) = drop d xs
      d = div (length xs) 2

pridajDoBVS :: (Ord a) => a -> BVS a -> BVS a
pridajDoBVS x PrazdnyBVS = VrcholBVS x PrazdnyBVS PrazdnyBVS
pridajDoBVS x t@(VrcholBVS y l r)
  | x == y    = t
  | x < y     = VrcholBVS y (pridajDoBVS x l) r
  | otherwise = VrcholBVS y l (pridajDoBVS x r)

zmazZBVS :: (Ord a) => a -> BVS a -> BVS a
zmazZBVS x t = t
--zmazZBVS x PrazdnyBVS = PrazdnyBVS
--zmazZBVS x (VrcholBVS y l r)

inorder :: BVS a -> [a]
inorder PrazdnyBVS = []
inorder (VrcholBVS x l r) = inorder l ++ [x] ++ inorder r

preorder :: BVS a -> [a]
preorder PrazdnyBVS = []
preorder (VrcholBVS x l r) = x:(preorder l) ++ preorder r

postorder :: BVS a -> [a]
postorder PrazdnyBVS = []
postorder (VrcholBVS x l r) = preorder l ++ preorder r ++ [x]
