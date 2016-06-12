module TypyHuff where

-- VrcholHuff zoznamZnakov pocetnost lavyPodstrom pravy Podstrom, kde
-- zoznamZnakov je zoznam resp. mnozina znakov v danom vrchole
-- pocetnost je sucet poctu pocetnosti znakov v zoznamZnakov
-- lavy a pravyPodstrom je zrejmy
-- list je Vrchol x px PrazdnyHuff PrazdnyHuff
data StromHuff = PrazdnyHuff |
                 VrcholHuff String Int StromHuff StromHuff
       deriving (Show, Read, Ord, Eq)

type ZozBitov = [Int]
