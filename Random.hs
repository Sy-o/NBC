module Random
(
    getRandomInexes,
    getTrainTest
) where

import System.Random
import Data.List

getRandomInexes :: RandomGen g => g -> Double -> Int -> [Int]
getRandomInexes gen splat len = getIndexes (neededLength*2) (randomRs (0,(len-1)) gen:: [Int])
  where 
    neededLength = round ((fromIntegral len::Double) * splat)
    getIndexes n listR = if length infinitListR >= neededLength then (take neededLength infinitListR) else getIndexes (n*2) listR
      where infinitListR = nub $ take n listR

getTrainTest :: Int -> [Int] -> ([Int],[Int])
getTrainTest totalLength train = (train, filter (\x -> not (elem x train)) [0..(totalLength-1)]) 






