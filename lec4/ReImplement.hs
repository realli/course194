module ReImplement where

-- re-implement fun1
fun1 :: [Integer] -> Integer
fun1 = (1+) . product . map (flip (-) 2) . filter even

-- fun2 re-implemention
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else (3*n+1))
