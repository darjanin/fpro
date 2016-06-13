module Obrazok where

-- DU 1 - 6k - 6.

type Obr = [[ Char ]]

nad :: Obr -> Obr -> Obr
nad = (++)

-- horizont·lna symetria
prevratH :: Obr -> Obr
prevratH = reverse

-- vertik·lna symetria
prevratV :: Obr -> Obr
-- prevratV obr = [reverse riadok | riadok <- obr]
prevratV obr = map reverse obr

-- zlepÌ dva obr·zky horizont·lne vedæa seba, ale musia byù rovnako vysokÈ
vedla :: Obr -> Obr -> Obr
vedla lavyObr pravyObr = [(lavyRiadok ++ pravyRiadok) |
                          (lavyRiadok, pravyRiadok) <- zip lavyObr pravyObr]

-- premaæuje x na . a naopak
vymenZnak :: Char -> Char
vymenZnak znak = if znak == 'x' then '.' else 'x'

-- premaæuje v matici x na . a naopak
vymenFarby :: Obr -> Obr

-- vymenFarby obr = [[ vymenZnak znak | znak <- riadok] | riadok <- obr]
vymenFarby obr = map (map vymenZnak) obr

-- vytlac na konzolu
zobrazObr :: Obr -> IO()
zobrazObr = putStr . concat . map (++ "\n")

----------- a toto mate dodefinovat...
zlozZnaky :: Char -> Char -> Char
zlozZnaky znak1 znak2 = if znak1 == '.' && znak2 == '.' then '.' else 'x'

zlozObrazky :: Obr -> Obr -> Obr
zlozObrazky obr1 obr2 = [[(zlozZnaky znak1 znak2) | (znak1, znak2) <- zip riadok1 riadok2] | (riadok1, riadok2) <- zip obr1 obr2]

obr1 = ["..xx", "xx..", ".x.x"]
obr2 = ["x.....","x.....","xxxxxx"]

-- zlozObrazky (prevratV obr2) (prevratH obr2) je ["xxxxxx","x....x","xxxxxx"]

obr3 = ["x..","x..","xxx"]
obr3inv = vymenFarby obr3
obr390  = prevratV obr3
obr390inv = vymenFarby obr390
obr3180 = prevratH obr390
obr3180inv = vymenFarby obr3180
obr3270 = prevratV obr3180
obr3270inv = vymenFarby obr3270

hornyrad = vedla (vedla (vedla obr3 obr390inv) obr3180) obr3270inv
dolnyrad = vedla (vedla (vedla obr3inv obr390) obr3180inv) obr3270
vytvor = nad hornyrad dolnyrad
