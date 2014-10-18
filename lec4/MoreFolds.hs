module MoreFolds where

-- return True only when odd number of True finded
xor :: [Bool] -> Bool
xor = odd . foldl (\b a -> if a then b+1 else b) 0

-- map implemented using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs


-- finding primes using sieve sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let removed = [ x | j <- [1..n], i <- [1..j], let x = i+j+2*i*j, x <=n]
                  in map ((1+) . (2*)) $ filter (`notElem` removed) [1..n]
