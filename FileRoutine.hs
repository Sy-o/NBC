module FileRoutine
(
  source,
  conduitParser,
  getMaybeData,
  unpackMaybeData
) where

import Control.Monad.Trans.Resource
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Data.List.Split.Internals
import Data.Char (isSpace)
import qualified Data.Conduit.List as CL

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

conduitParser:: Conduit String (ResourceT IO) (Maybe ([Double], String)) 
conduitParser = awaitForever $ yield . getObject . splitOn "," 
    where 
        getObject a = maybeVector (mapM tryParse (init a), trim (last a))
            where 
                maybeVector (Just x, c) = Just (x,c)
                maybeVector (Nothing, _) = Nothing


tryParse :: (Read a) => String -> Maybe a  
tryParse s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

trim :: String -> String
trim = f . f 
  where f = reverse . dropWhile isSpace

getMaybeData path = runResourceT $ (source path) $= conduitParser $$ CL.consume

unpackMaybeData d = case d of 
    Nothing -> error "Wrong CSV File Format!"
    Just x -> x

