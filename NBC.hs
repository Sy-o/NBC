module NBC
(
  addClassFreq,
  normalizeClassesFreq,
  addObjectToClass,
  normalizeClassesFeats,
  calcDispersion,
  classify,
  validate
) where

import qualified Data.Map as Map
import Data.List

-------------------------------- Train part --------------------------------------

addClassFreq :: Num a => String -> Map.Map String a -> Map.Map String a  
addClassFreq c map' = Map.insertWith (+) c 1 map'

normalizeClassesFreq :: Fractional a => a -> Map.Map String a -> Map.Map String a
normalizeClassesFreq objCount map' = Map.map (/objCount) map' 



addObjectToClass :: Num a => String -> [a] -> Map.Map String [(a,a)] -> Map.Map String [(a,a)] 
addObjectToClass c obj map' = Map.insertWith (f) c (toupleFromList obj) map'
  where toupleFromList o = map (\i -> (i,1)) o
        f x y = zipWith (\(i,q1) (j,q2) -> ((i+j),(q1+q2))) x y

normalizeClassesFeats :: Fractional a => Map.Map String [(a,a)] -> Map.Map String [(a,a)] 
normalizeClassesFeats fMap = Map.map (map (\(m,q) -> (m/q,0))) fMap

getClassObj :: String -> [([a], String)] -> [[a]]
getClassObj c objs = transpose . map (fst) $ filter (\(feats,cl) -> c == cl ) objs


calcDispersion :: Fractional a => [([a], String)] -> Map.Map String [(a,a)] -> Map.Map String [(a,a)]  
calcDispersion objs fMap = Map.mapWithKey (dispForClass) fMap
  where 
    dispForClass k v = zipWith (\(m,q) xi -> (m, (disp xi m))) v (getClassObj k objs)
      where 
        disp x m = (/((fromIntegral $ length x)-1)) $ sum $ map (^2) $ map (\i -> i - m) x

------------------------------- Test part -------------------------------------------
classify :: (Floating a, Ord a) => [[a]] -> Map.Map String a -> Map.Map String [(a,a)] -> [String]
classify xs classesFreq classes = map (classifyObj) xs
  where
    classifyObj x = fst $ findMaxInMap $ Map.mapWithKey (probForClass) classes
      where
        probForClass k val = (classesFreq Map.! k) * ( product ( zipWith (featureProb) val x ))
          where
            featureProb (m,q) v = exp(-((v - m)^2 / (2 * q^2))) / (sqrt 2*pi*q^2)   

findMaxInMap :: Ord a => Map.Map t a -> (t,a)
findMaxInMap map' = maximumBy (\(k,v) (k1,v1) -> compare v v1) $ Map.toList map'

validate expected actual = (/(fromIntegral (length actual))) $ sum $ zipWith (\a b -> if a == b then 0 else 1) expected actual