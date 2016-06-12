sqr x = x * x

subseqs1 [] = [[]]
subseqs1 (x:xs) = subseqs1 xs ++ map (x:) (subseqs1 xs)

subseqs2 [] = [[]]
subseqs2 (x:xs) = xss ++ map (x:) xss
  where xss = subseqs2 xs

foo1 n = sum (take n primes)
  where
    primes = [x | x<-[2..], divisors x == [x]]
    divisors x = [d | d<-[2..x], x `mod` d == 0]

foo2 n = sum (take n primes)
primes = [x | x<-[2..], divisors x == [x]]
divisors x = [d | d <- [2..x], x `mod` d == 0]

sucet :: (Num a) => [a] -> a
sucet = foldl (+) 0

myfoldl1 f vys [] = vys
myfoldl1 f vys (x:xs) = myfoldl1 f (f vys x) xs

mojfoldl2 f vys [] = vys
mojfoldl2 f vys (x:xs) = y `seq` mojfoldl2 f y xs
  where y = f vys x
