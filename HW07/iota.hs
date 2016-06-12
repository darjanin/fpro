X = \x.((x S) K) = \x.(x S K)
S = \xyz.(xz) (yz)
K = \xy.x

K
= X (X (X X))
= \x.(x S K) (X (X X))
= ((X (X X)) S K)
= ((\x.(x S K) (X X)) S K)
= (\x.(x S K) (X X) S K)
= (((X X) S K) S K)
= X X S K S K
= \x.(x S K) X S K S K
= (X S K) S K S K
= X S K S K S K
= \x.(x S K) S K S K S K
= (S S K) K S K S K
= (S S K K) S K S K        -- od tohto riadku zatvorky pouzivam na zvyraznenie co ratam
= (S K K K) S K S K
= (K K K) K S K S K
= (K K S) K S K
= (K K S) K
= K K                      -- K chce 2 argumenty ale bez ohlad na druhy stale vrati K
= K

S
= X (X (X (X X)))
-- dokaz S vyuzije predchadzaujuci dokaz
= \x.(x S K) (X (X (X X)))
-- vyssie je ukazane ze (X (X (X X))) = K
= \x.(x S K) K
= K S K
= S
