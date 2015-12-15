module Trash
(
    getClassesFreq,
    getFeaturesFreq
)where

import NBC
import qualified Data.Map as Map

getClassesFreq :: [([Double],String)] -> Map.Map String Double
getClassesFreq x = normalizeClassesFreq  (fromIntegral (length x ) ::Double) $ foldl (\acc (_,c) -> addClassFreq c acc) Map.empty x

getFeaturesFreq :: Fractional a => [([a],String)] -> Map.Map String [(a,a)] 
getFeaturesFreq x = normalizeClassesFeats $ foldl (\acc (xs,c) -> addObjectToClass c xs acc) Map.empty x