module VyvazenyStrom where
import TypyTree
-- data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Read)
jeVyvazeny :: Tree a -> Bool
jeVyvazeny Empty = True
jeVyvazeny (Branch _ a b) = rozdielBratov a b && jeVyvazeny a && jeVyvazeny b
  where
    rozdielBratov a b = (abs (vyskaBrata a - vyskaBrata b) <= 1)
    vyskaBrata Empty = 0
    vyskaBrata (Branch _ a b) = 1 + max (vyskaBrata a) (vyskaBrata b)

