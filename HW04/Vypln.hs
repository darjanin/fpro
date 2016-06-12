module Vypln where
import VyplnTypy    -- obsahuje Pos a Domino. V samostatnom subore (je prilozeny)  aby sa lahsie testovalo...
-- reprezentacia pozicie policka
-- data
--    Pos = P Int Int
--      deriving (Eq, Ord, Show, Read)
-- reprezentacia domina
-- orientovaneho horizontalne resp. vertikalne,
-- ktoreho lavy horny roj je na pozicii Pos
--data
--    Domino = Horiz Pos | Vert Pos
--     deriving (Eq, Show, Read)
--ries :: Int -> Int -> Pos -> Pos -> [Domino]
ries m n (P x1 y1) (P x2 y2)
  | jeRiesitelne m (P x1 y1) (P x2 y2) == False = []
  | otherwise = [Vert (P 1 2)]

jeRiesitelne :: Int -> Pos -> Pos -> Bool
jeRiesitelne m p1 p2 = moznost1 || moznost2
  where
    zratajHodnotu (P x y) = (x * m) + y + 1
    moznost1 = even (zratajHodnotu p1) && odd (zratajHodnotu p2)
    moznost2 = even (zratajHodnotu p2) && odd (zratajHodnotu p1)