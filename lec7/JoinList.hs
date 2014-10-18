{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                     | Single m a
                     | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- tag will get the annotation of JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- do not think Empty is useful, so get rid of it
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ l = l
l +++ Empty = l
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

-- here is the indexJ , zero based
indexJ :: (Sized b, Monoid b) =>
           Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Append m _ _)
    | i >= (getSize . size) m = Nothing
indexJ i (Append _ l1 l2)
    | i < ltag = indexJ i l1
    | otherwise = indexJ (i - ltag) l2
        where ltag = getSize . size $ tag l1
indexJ i (Single m x)
    | i < (getSize . size) m = Just x
    | otherwise = Nothing

-- drop 
dropJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
dropJ i l
    | i <= 0 = l
    | i >= (getSize . size . tag) l = Empty -- here we include the sitution of Single
dropJ i (Append _ l1 l2)
    | i <= (getSize . size . tag) l1 = dropJ i l1 +++ l2
    | otherwise = dropJ (i - (getSize . size. tag) l1) l2

-- here comes take
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ i l
    | i <= 0 = Empty
    | i >= getAn l = l -- include Single already
        where getAn = getSize . size . tag
takeJ i (Append _ l1 l2)
    | i <= getAn l1 = takeJ i l1
    | otherwise = l1 +++ takeJ (i - getAn l1) l2
        where getAn = getSize . size . tag



-- test for Scrabble
--
--
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, 1) s

jlFromList :: [String] -> JoinList (Score, Size) String
jlFromList [] = Empty
jlFromList (x:[]) = scoreSizeLine x
jlFromList ls = let len = length ls
                    half = len `div` 2
                in (jlFromList $ take half ls) +++ (jlFromList $ drop half ls)

-- new instance for Buffer
instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = jlFromList . lines
  line         = indexJ
  -- n may be zero based
  replaceLine n l b = ahead +++ scoreSizeLine l +++ atail
      where ahead = takeJ (n) b
            atail = dropJ (n+1) b
  numLines     = getSize . snd . tag
  value        = getScore . fst . tag



jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
