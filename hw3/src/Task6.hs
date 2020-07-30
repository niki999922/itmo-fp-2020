{-# LANGUAGE RankNTypes #-}

module Task6 where

import Lens.Micro
import Task5

-- Go to the subdirectory with the specified name.
cd :: String -> Traversal' FS FS
cd path = dirContents . each . filtered isNeedDir
  where
    isNeedDir :: FS -> Bool
    isNeedDir fs =
      case fs ^? dirName of
        Just x  -> x == path
        Nothing -> False

-- Get a list of directory contents names.
ls :: Traversal' FS FilePath
ls = dirContents . each . name

-- Get the name of a specific File, if one exists.
file :: String -> Traversal' FS String
file path = dirContents . each . (filtered isNeedFile) . fileName
  where
    isNeedFile :: FS -> Bool
    isNeedFile fs =
      case fs ^? fileName of
        Just x  -> x == path
        Nothing -> False
