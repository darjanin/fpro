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

--ktorá zistí, či je mobil m vyvážený.  Mobil je vyvážený keď je vyvážený jeho pravý aj ľavý podmobil.
--a hmotnosť laveho podmobilu * d1 == hmotnosť pravého podmobilu * d2
--Príklad
--vyvazenyMobil (Rameno (List 10) (List 5) 5 10) vráti True
--vyvazenyMobil (Rameno (List 10) (List 5) 5 9) vráti False
--vyvazenyMobil (List 10) vráti True