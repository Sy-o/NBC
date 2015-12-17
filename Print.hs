module Print
(
  printResult,
  printMatrix
) where

import qualified Data.Map as Map 

printResult featFreq = mapM_ (\(k,v) -> print ("class " ++ k ++ " : " ++ (show v))) $ Map.toList featFreq

printMatrix m = mapM_ print m 