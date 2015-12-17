module FileRoutine
(
  source,
  conduitParser,
  getMaybeData,
  unpackMaybeData,
  writeResultToFile
) where

import Control.Monad.Trans.Resource
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Data.List.Split.Internals
import Data.Char (isSpace)
import Data.List
import qualified Data.Conduit.List as CL

-----------------------------  Read input -------------------------------------

source:: FilePath -> Source (ResourceT IO) String
source path = do
  bracketP
     (openFile path ReadMode)
     (\handle -> putStrLn "Closing handle" >> hClose handle)
     readByLine
  where
    readByLine handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetLine handle
                yield c
                readByLine handle

conduitParser:: String -> Conduit String (ResourceT IO) (Maybe ([Double], String)) 
conduitParser splitter = awaitForever $ yield . getObject . splitOn splitter 
    where 
        getObject a = maybeVector (mapM tryParse (init a), trim (last a))
            where 
                maybeVector (Just x, c) = Just (x, c)
                maybeVector (Nothing, _) = Nothing


tryParse :: (Read a) => String -> Maybe a  
tryParse s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

trim :: String -> String
trim = f . f 
  where f = reverse . dropWhile isSpace

getMaybeData path splitter = runResourceT $ (source path) $= (conduitParser splitter) $$ CL.consume

unpackMaybeData d = case d of 
    Nothing -> error "Wrong CSV File Format!"
    Just x -> x


-----------------------------------  Write result ----------------------------------------

conduitWriter:: Conduit (String,[(Double,Double)]) IO String 
conduitWriter = awaitForever $ yield . convertToNiceStr
  where 
    convertToNiceStr (k,v)= "class " ++ k ++ " : \n " ++ show' v ++ "\n\n"
      where
        show' x = intercalate " - " $ map (\(m,q) -> "(" ++ show m ++ "; " ++ show q ++ ")") x

fileSink :: Handle -> Sink String IO ()
fileSink handle = CL.mapM_ (hPutStrLn handle)


writeResult handle result = CL.sourceList result $= conduitWriter $$ fileSink handle

writeResultToFile path indexes result = withFile path WriteMode $ \h -> do 
  writeResult h result
  hPrint h "Train indexes:"
  hPrint h $ intercalate ", " (map show indexes)
  
