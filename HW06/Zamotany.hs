--Do modulu Zamotany napíšte funkciu zamotanyMobil
module Zamotany where
import MobilTypy
--                 d1           d2
--        |--------------|-------|
--        |                           |
--
--                  hmotnost              lavy pravy d1 d2
--data Mobil = List Int | Rameno Mobil Mobil Int Int
zamotanyMobil :: Mobil -> Boolean
zamotanyMobil m =

vyvazenyMobil :: Mobil -> Bool
vyvazenyMobil (List x) = True
vyvazenyMobil (Rameno (List x1) (List x2) d1 d2) = x1 * d1 == x2 * d2
vyvazenyMobil (Rameno m1 m2 d1 d2) = vyvazenyMobil m1 && vyvazenyMobil m2 && (hmotnost m1) * d1 == (hmotnost m2) * d2

hmotnost :: Mobil -> Int
hmotnost (List x) = x
hmotnost (Rameno m1 m2 d1 d2) = hmotnost m1 + hmotnost m2

-- mobil = (Rameno (Rameno (List 4) (List 8) 4 2) (Rameno (List 3) (List 3) 3 3) 2 4)

--ktorá zistí, či sa v danom mobile (za predpokladu jeho vyváženosti)
--pri otáčani ktorej koľvek časti táto nezrazí z nejakou inou časťou mobilu,
--čo by viedlo k k zamotaniu mobilu.
--Predpokladajte, že všetky lanká, na ktorych visia podmobily na konci ramien sú rovnako dlhé.