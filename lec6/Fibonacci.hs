{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-- fib
fib :: Integer -> Integer
fib 0  = 0
fib 1  = 1
fib n = fib (n-2) + fib (n-1)

-- fibsl, out put fibonaccis infinite
fibsl :: [Integer]
fibsl = map fib [1..]

-- fibs2 generate fibonacci in O(n)
fibs2 :: [Integer]
fibs2 = 0 : 1 :
        zipWith (+) fibs2 (tail fibs2)
        
-- data type Stream
data Stream a = a :+: (Stream a) deriving (Eq)

infixr 5 :+:

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- translate Stream to list
streamToList :: Stream a -> [a]
streamToList (x :+: xs) = x : streamToList xs

-- infite Stream
streamRepeat :: a -> Stream a
streamRepeat x = x :+: streamRepeat x

-- map function ro Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :+: xs)  = f x :+: streamMap f xs

--  generates a Stream from a “seed” of type a, which is the
--  first element of the stream, and an “unfolding rule” of type a -> a
--  which specifies how to transform the seed into a new seed, to be
--  used for generating the rest of the stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f init = let next = f init
                        in init :+: streamFromSeed f next

-- contains the infinite list of natural numbers 0,1,2...
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- interleave Stream 
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :+: xs) (y :+: ys) = x :+: y :+: interleaveStreams xs ys

ruler' :: Stream Integer
ruler' = 1 :+: interleaveStreams (streamRepeat 0) (streamMap (+1) ruler')

ruler = 0 :+: ruler'
     
-- optinal
--
--
--
x :: Stream Integer
x = 0 :+: 1 :+: streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = n :+: streamRepeat 0
    negate = streamMap (0 -)
    (+) (x :+: xs) (y :+: ys) = (x+y) :+: (xs + ys)
    (a0 :+: as) * b@(b0 :+: bs) = a0*b0 :+: (streamMap (a0*) bs + (as*b))

instance Fractional (Stream Integer) where
    (/) aa@(a :+: as) bb@(b :+: bs) = (a `div` b) :+: streamMap (`div` b) (as - (aa/bb)*bs) 

fibs3 :: Stream Integer
fibs3 = (0 :+:1 :+: streamRepeat 0) / (1 :+: -1 :+: -1 :+: streamRepeat 0)

-- Data type for Matrix 2x2
type Matrix = (Integer, Integer, Integer, Integer)

instance Num Matrix where
    (a1,a2,a3,a4) * (b1,b2,b3,b4) =
        ((a1*b1+a2*b3),(a1*b2+a2*b4),(a3*b1+a4*b3),(a3*b2+a4*b4))

fib4' :: Integer -> Integer
fib4' = go . (^)  ((1,1,1,0) :: Matrix)
    where go (_,x,_,_) = x

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 x
    | x <0 = - fib4' (-x)
    | otherwise = fib4' x
