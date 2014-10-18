{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int deriving (Show, Num, Eq, Ord)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

-- caculate the score according to the Scrabble
score :: Char -> Score
score c
    | toUpper c `elem` "AEILNORSTU" = 1
    | toUpper c `elem` "DG" = 2
    | toUpper c `elem` "BCMP" = 3
    | toUpper c `elem` "FHVWY" = 4
    | toUpper c `elem` "K" = 5
    | toUpper c `elem` "JX" = 8
    | toUpper c `elem` "QZ" = 10
    | otherwise = 0

scoreString :: String -> Score
scoreString = foldl (flip ((+). score)) 0
