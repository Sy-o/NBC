import NBC
import Trash
import FileRoutine
import Print

import Prelude hiding (catch) 
import System.IO
import System.IO.Error hiding(try, catch, ioError)
import Control.Exception


main = do 
  x <- getMaybeData "source1.txt"
  let
    objs = unpackMaybeData $ sequence x
    classesFreq = getClassesFreq objs
    featuresFreq = getFeaturesFreq objs
    in printMatrix $ classify (map (fst) objs) classesFreq featuresFreq



    
