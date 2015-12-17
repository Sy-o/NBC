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


main = do 
  newGen <- R.getStdGen
  let 
    (train,test) = getTrainTest 15 $ getRandomInexes newGen 0.8 15
    in print ("train indexes: " ++ show train ++ " test indexes :" ++ show test )
  
  
  
  -- x <- getMaybeData "source2.txt"
  -- let
  --   objs = unpackMaybeData $ sequence x
  --   classesFreq = getClassesFreq objs
  --   featuresFreq = getFeaturesFreq objs
  --   classifyResult = classify (map (fst) objs) classesFreq featuresFreq
  --   in printClassifyError $ validate (map (snd) objs) classifyResult




    
