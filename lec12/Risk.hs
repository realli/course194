{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (liftM)
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

-- roll n times
rollTimes :: Army -> Rand StdGen [DieValue]
rollTimes n = liftM (sortBy (flip compare)) . sequence $ replicate n die

----- a single battle
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a b) = do
    valueA <- rollTimes unitA 
    valueB <- rollTimes unitB 
    let valueList = zipWith (\x y ->
            if x > y then (1,0) else (0,1))
            valueA valueB
        hurtA = sum . map fst $ valueList
        hurtB = sum . map snd $ valueList
    return (Battlefield (a - hurtA) (b - hurtB))
    where unitA = min (a-1) 3
          unitB = min 2 b

-- invade , battle until no defenders and attackers
invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
    nextBattle <- battle b
    if attackers nextBattle < 2 || defenders nextBattle <= 0
    then return nextBattle
    else invade nextBattle
    

-- successProb ,, simulate 1000 time invade ,caculate the probility
successProb :: Battlefield -> Rand StdGen Double
successProb b = liftM ((/ 1000) . go) . sequence . replicate 1000 $ invade b
    where go = fromIntegral . length . filter ((<=0) . defenders)
