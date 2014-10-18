type Peg = String
type Move = (Peg, Peg)

-- move n stack from a to b using c as temp
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0 = []
    | n == 1 = [(a, b)]
    | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- move n stack using r Pegs
hanoiR :: Integer -> [Peg] -> [Move]
hanoiR 0 _ = []
hanoiR 1 (p1:p2:prest) = [(p1,p2)]
hanoiR n (p1:p2:p3:prest) = hanoiR k (p1:p3:p2:prest) ++
                            hanoiR (n-k) (p1:p2:prest) ++
                            hanoiR k (p3:p2:p1:prest)
        where k = if null prest then n-1 else n - floor (sqrt (fromInteger (2*n)) + 0.5)
