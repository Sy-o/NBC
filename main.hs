import NBC
import CSVParse
import Trash

import Prelude hiding (catch) 
import System.IO
import System.IO.Error hiding(try, catch, ioError)
import Control.Exception

import qualified Data.Map as Map 

printResult featFreq = mapM_ (\(k,v) -> print ("class " ++ k ++ " : " ++ (show v))) $ Map.toList featFreq

getData sourceData s = sCSV (getMaybeData sourceData s::Maybe [([Double],String)])
   where sCSV (Nothing) = error "Wrong CSV File Format"
         sCSV (Just a) = a

main = do
  sourceData <- readFile "source1.txt" `catch` handlerForFileRoutine "source1.txt"
  let 
    x = getData sourceData ","
    classesFreq = getClassesFreq x
    featuresFreq = getFeaturesFreq x
    in printResult featuresFreq


    
