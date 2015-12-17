import NBC
import Trash
import FileRoutine
import Print
import Random
import Args

import Prelude hiding (catch) 
import System.IO
import System.IO.Error hiding(try, catch, ioError)
import Control.Exception
import System.Random as R
import qualified Data.Map as Map
import Control.Monad.Reader
import System.Environment
import System.Console.CmdArgs

options = InputOptions{
  splat = 0.7 &= help "break source data",
  sourceFile = "source2.txt" &= help "file path for sorce data",
  columnSplitter = "," &= help "column splitter in csv file"
}

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
  args' <- cmdArgs options
  x <- getMaybeData (sourceFile args') (columnSplitter args')
  let 
    objs = unpackMaybeData $ sequence x
    splat_ = splat args'
    (trainObj, testObj, train, test) = runReader getSeparatedData (objs, splat_, newGen)

    classesFreq = getClassesFreq trainObj
    featuresFreq = getFeaturesFreq trainObj
    classifyResult = classify (map (fst) testObj) classesFreq featuresFreq

    in do
      writeResultToFile "output.txt" train $ Map.toList featuresFreq
      printClassifyError $ validate (map (snd) testObj) classifyResult
      putStrLn "\nResult of classification: "
      print classifyResult
      putStrLn "\nTest indexes:"
      print test
      putStrLn "------------------------------------------------"
  
