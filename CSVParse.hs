module CSVParse
(
  handlerForFileRoutine,
  getMaybeData
)where

import System.IO.Error
import Data.List.Split.Internals
import Data.Maybe

handlerForFileRoutine name e
  | isDoesNotExistError e = error ("File "++ name ++" does not exist") 
  | isAlreadyInUseError e = error ("File " ++ name ++" is already used ")
  | isPermissionError e = error   ("Permission error while trying open file "++ name) 
  | otherwise = ioError e 

getStringMatrix :: String -> String -> [[String]]
getStringMatrix source splitter = map (splitOn splitter) $ splitOn "\n" source

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, _)] -> Just x
    _        -> Nothing

getMaybeData source splitter = getMaybeMatrix $ cutM $ getStringMatrix source splitter

cutM matrix = map (\v -> (init v, last v)) matrix

getMaybeMatrix :: [([String], String)] -> Maybe [([a], String)] 
getMaybeMatrix m = let 
  classes = map (snd) m
  objs = map (fst) m
  mayBeObjs = unpackMabeMatrix $ map (map readMaybe) objs
    in joinObjsAndClasses mayBeObjs classes

joinObjsAndClasses :: Maybe [[a]] -> [String] -> Maybe [([a],String)]
joinObjsAndClasses o c
  | Nothing _ = Nothing
  | (Just o) c = Just (zipWith (\oi ci -> (oi, ci)) o c)   


unpackMabeVector :: [Maybe a] -> Maybe [a]
unpackMabeVector v = mV [] v
   where mV acm [] = Just (reverse acm)
         mV acm ((Just a):xs) = mV (a:acm) xs
         mV acm (Nothing:xs) = Nothing

unpackMabeMatrix :: [[Maybe a]] -> Maybe [[a]]
unpackMabeMatrix m = unpackMabeVector $ map unpackMabeVector m

  