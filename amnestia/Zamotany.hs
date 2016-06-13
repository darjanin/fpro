module Vyvazeny where
import MobilTypy
--                 d1           d2
--        |--------------|-------|
--        |                           |
--
--                  hmotnost              lavy pravy d1 d2
--data Mobil = List Int | Rameno Mobil Mobil Int Int
vyvazenyMobil :: Mobil -> Bool
vyvazenyMobil (List x) = True
vyvazenyMobil (Rameno (List x1) (List x2) d1 d2) = x1 * d1 == x2 * d2
vyvazenyMobil (Rameno m1 m2 d1 d2) = vyvazenyMobil m1 && vyvazenyMobil m2 && (hmotnost m1) * d1 == (hmotnost m2) * d2

hmotnost :: Mobil -> Int
hmotnost (List x) = x
hmotnost (Rameno m1 m2 d1 d2) = hmotnost m1 + hmotnost m2

zamotanyMobil :: Mobil -> Bool
zamotanyMobil mobil = (vyvazenyMobil mobil) && (not . nezamota $ mobil)

nezamota :: Mobil -> Bool
nezamota (List _) =  True
nezamota (Rameno (List _) (List _) _ _ ) = True
nezamota (Rameno (List _) (Rameno m1 m2 d1 d2) r1 r2) = max d1 d2 < r1 + r2 && nezamota (Rameno m1 m2 d1 d2)
nezamota (Rameno (Rameno m1 m2 d1 d2) (List _) r1 r2) = max d1 d2 < r1 + r2 && nezamota (Rameno m1 m2 d1 d2)
nezamota (Rameno (Rameno m11 m12 d11 d12) (Rameno m21 m22 d21 d22) r1 r2) =
  max d11 d12 + max d21 d22 < r1 + r2 &&
  nezamota (Rameno m11 m12 d11 d12) &&
  nezamota (Rameno m21 m22 d21 d22) &&
  nezamota (Rameno m12 m21 0 (r1 + r2 - d12 - d21)) &&
  nezamota (Rameno m12 m22 0 (r1 + r2 - d12 - d22)) &&
  nezamota (Rameno m11 m21 0 (r1 + r2 - d11 - d21)) &&
  nezamota (Rameno m11 m22 0 (r1 + r2 - d11 - d22))

m1 = List 10
m2 = Rameno m1 (List 5) 5 10
m3 = Rameno m2 m1 10 15   -- vyvazeny
m4 = Rameno m2 m1 15 10   -- nevyvazeny
m5 = Rameno m3 m4 3 3
m6 = Rameno (List 5) (List 10) 4 2 -- vyvazeny, nezamota
m7 = Rameno (List 2) (List 1) 2 4  -- vyvazeny, nezamota
m8 = Rameno m6 m7 1 5 -- vyvazeny, zamota
m9 = Rameno m8 m8 10 10 -- vyvazeny, zamota
m10 = Rameno (List 2) (List 4) 4 2 -- vyvazeny, nezamota
m11 = Rameno (List 1) (List 3) 3 1 -- vyvazeny, nezamota
m12 = Rameno m10 m11 4 6 -- vyvazeny, nezamota
m13 = Rameno m12 m12 7 7 -- vyvazeny, zamota
