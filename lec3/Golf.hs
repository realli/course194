module Golf where

import Data.List (tails, group, sort, maximumBy, transpose)
import Data.Char (chr)
import Data.Function (on)

skip :: [a] -> Int -> [a]
skip [] n = []
skip (l:ls) n = l : skip (drop n ls) n

skips :: [a] -> [[a]]
skips ls = init $ zipWith skip (tails ls) [0..]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (a:[]) = []
localMaxima (a:b:[]) = []
localMaxima (a:b:c:ls) = if b > a && b > c
                         then b : localMaxima (c:ls)
                         else localMaxima (b:c:ls)

histogram :: [Int] -> String
histogram ls = unlines rr
    where go _ [] = []
          go (n:ns) ls@((e,l):re)
            | n == e = (show n ++ "=" ++ replicate l '*') : go ns re        
            | otherwise = (show n ++ "=") : go ns ls
          go2 = length . maximumBy (compare `on` length)
          r = go [0..9] . map (\x -> (head x, length x)) . group . sort $ ls
          len = go2 r
          rr = transpose $ map (\ll -> reverse (ll ++ replicate (len - length ll) ' ')) r
