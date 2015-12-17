{-# LANGUAGE DeriveDataTypeable #-}

module Args
(
    parseArgs,
    InputOptions(..)
) where

import Data.List.Split.Internals
import Data.String
import Data.Typeable
import Data.Data

parseArgs args = map (splitOn " ") args

data InputOptions = InputOptions{
  splat :: Double,
  sourceFile :: String,
  columnSplitter :: String
} deriving (Show, Data, Typeable)