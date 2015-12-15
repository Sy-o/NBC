import NBC;
import CSVParse;
import Trash;

import qualified Data.Map as Map 

getData sourceData s = sCSV (getMaybeData sourceData s::Maybe [([Double],String)])
   where sCSV (Nothing) = error "Wrong CSV File Format"
         sCSV (Just a) = a

main = do
  sourceData <- readFile "source1.txt" `catch` handlerForFileRoutine "source1.txt"
  let 
    x = getData sourceData

    
