import NBC
import Trash
import FileRoutine
import Print
import Random

import Prelude hiding (catch) 
import System.IO
import System.IO.Error hiding(try, catch, ioError)
import Control.Exception
import System.Random as R
import qualified Data.Map as Map


main = do 
  x <- getMaybeData "source2.txt"
  newGen <- R.getStdGen
  let 
    objs = unpackMaybeData $ sequence x
    (train,test) = getTrainTest 15 $ getRandomInexes newGen 0.6 (length objs)
    trainObj = getElems train objs
    testObj = getElems test objs
    classesFreq = getClassesFreq trainObj
    featuresFreq = getFeaturesFreq trainObj
    classifyResult = classify (map (fst) testObj) classesFreq featuresFreq
    in do
      printClassifyError $ validate (map (snd) testObj) classifyResult
      putStrLn "\nResult of classification: "
      print classifyResult
      putStrLn "\nTest indexes:"
      print test
      putStrLn "---------------"


      writeResultToFile "output.txt" train $ Map.toList featuresFreq




    
