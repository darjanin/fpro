module Zalom where
import Data.List
import Data.List.Split

zalom :: Int -> Int -> String -> String
zalom riadokSirka odsadenie text = concat . nub $ spojeneOdstavce
  where
    vysledok = spracuj riadokSirka odsadenie (lines text)
    upravaMedzier = [if ((length $ words $ last odstavec) == 1)
      then (take (length odstavec - 1) odstavec) ++ [rightPad riadokSirka (last odstavec)]
      else odstavec | odstavec <- vysledok]
    spojeneOdstavce = [unlines odstavec | odstavec <- upravaMedzier]

spracuj riadokSirka odsadenie riadky = [removeLastSpace $ spracujVetu riadokSirka (split (oneOf " ") riadok) (medzery odsadenie) | riadok <- riadky]
  where
    removeLastSpace xs = [ if (last x == ' ') then take (length x - 1) x else x | x <- xs]

spracujVetu _ [] w = [w]
spracujVetu riadokSirka (x:xs) w =
  if length (x ++ w) > riadokSirka
  then
    if length x > riadokSirka
    then [w] ++ [x] ++ spracujVetu riadokSirka xs ""
    else [w] ++ spracujVetu riadokSirka xs (if x == " " then "" else x)
  else spracujVetu riadokSirka xs (w ++ x)

medzery n = concat $ replicate n " "
rightPad riadokSirka s = s ++ medzery (riadokSirka - length s)

line = "Tento text sa ma zarovnat na sirku 15."

text = "Tento text sa ma zarovnat na sirku 15.\nPrvy riadok odstavca ma byt odsadeny o 4 znaky.\n\n\nViac prazdnych riadkov za sebou je ako kebol iba jeden."
v1 = "Tento text sa ma zarovnat na sirku 16.\nPrvy riadok odstavca ma byt odsadeny o 4 znaky.\n\nViac prazdnych riadkov za sebou je ako keby bol iba jeden.\n"
