module Print
(
  printResult,
  printMatrix,
  printClassifyError
) where

import qualified Data.Map as Map 

printResult featFreq = mapM_ (\(k,v) -> print ("class " ++ k ++ " : " ++ (show v))) $ Map.toList featFreq

printMatrix m = mapM_ print m 

printClassifyError er = print $ "Classify error is " ++ show er