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
import Control.Monad.Reader


getSeparatedData :: RandomGen g => Reader ([([a],String)], Double, g) ([([a],String)],[([a],String)], [Int], [Int])
getSeparatedData = do
    (objs, splat, gen) <- ask
    let
      (train,test) = getTrainTest (length objs) $ getRandomInexes gen splat (length objs)
      trainObj = getElems train objs
      testObj = getElems test objs
    return (trainObj, testObj, train, test)

main = do 
  newGen <- R.getStdGen
  x <- getMaybeData "source2.txt" ","
  let 
    objs = unpackMaybeData $ sequence x
    
    (trainObj, testObj, train, test) = runReader getSeparatedData (objs, 0.6, newGen)

    classesFreq = getClassesFreq trainObj
    featuresFreq = getFeaturesFreq trainObj
    classifyResult = classify (map (fst) testObj) classesFreq featuresFreq
    in do
      printClassifyError $ validate (map (snd) testObj) classifyResult
      putStrLn "\nResult of classification: "
      print classifyResult
      putStrLn "\nTest indexes:"
      print test
      putStrLn "------------------------------------------------"

      writeResultToFile "output.txt" train $ Map.toList featuresFreq




    
