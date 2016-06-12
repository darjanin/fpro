module HuffZakoduj where
import TypyHuff
import Data.List

-- data StromHuff = PrazdnyHuff |
--                  VrcholHuff String Int StromHuff StromHuff
--        deriving (Show, Read, Ord, Eq)
--
-- type ZozBitov = [Int]

-- k danemu stringu s vytvori dvojicu
-- obsahujucu Huffmanov strom a nim zakodovany zakodovany string s
huffZakoduj :: String -> (StromHuff, ZozBitov)
huffZakoduj xs = (t, encode t xs)
  where
    t = head . huffTree . sortHuffList . pairs $ xs

encode :: StromHuff -> String -> ZozBitov
encode t@(VrcholHuff ys _ _ _) xs = if length ys == 1 then (replicate (length xs) 0) else foldl (\acc x -> acc ++ encodeChar t x) [] xs

encodeChar :: StromHuff -> Char -> ZozBitov
encodeChar (VrcholHuff xs _ PrazdnyHuff PrazdnyHuff) x = []
encodeChar (VrcholHuff _ _ l@(VrcholHuff lxs _ _ _) r@(VrcholHuff rxs _ _ _)) x
  | elem x lxs = 0:(encodeChar l x)
  | elem x rxs = 1:(encodeChar r x)
  | otherwise = []

huffTree :: [StromHuff] -> [StromHuff]
huffTree [x] = [x]
huffTree (x:y:xs) = huffTree . sortHuffList $ (merge x y):xs

merge :: StromHuff -> StromHuff -> StromHuff
merge x@(VrcholHuff xc xn _ _) y@(VrcholHuff yc yn _ _) = VrcholHuff (xc ++ yc) (xn + yn) x y

pairs :: String -> [StromHuff]
pairs xs = foldl (\acc x -> (VrcholHuff [x] (count x xs) PrazdnyHuff PrazdnyHuff):acc) [] (nub xs)

sortHuffList :: [StromHuff] -> [StromHuff]
sortHuffList = sortBy (\(VrcholHuff _ x _ _) (VrcholHuff _ y _ _) -> compare x y)

count :: Char -> String -> Int
count c xs = foldl (\acc x -> acc + if x == c then 1 else 0) 0 xs

-- k danej dvojici (hs,bs) :: (StromHuff, ZozBitov) vytvorte string,
-- ktory je zakodovany postupnostou bitov bs Huffmanovym stromom hs
huffDekoduj :: (StromHuff, ZozBitov) -> String
huffDekoduj (hs@(VrcholHuff xs _ _ _), bs) = if length xs == 1 then (concat $ replicate (length bs) xs) else decode hs bs

decode :: StromHuff -> ZozBitov -> String
decode t z = if length restZ > 0 then letter:(decode t restZ) else [letter]
  where
    decoded = decodeChar t z
    letter = fst decoded
    restZ = snd decoded

decodeChar :: StromHuff -> ZozBitov -> (Char, ZozBitov)
decodeChar (VrcholHuff [y] _ PrazdnyHuff PrazdnyHuff) xs = (y, xs)
decodeChar (VrcholHuff _ _ l r) (x:xs)
  | x == 0 = decodeChar l xs
  | x == 1 = decodeChar r xs
