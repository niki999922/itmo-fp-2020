{-# LANGUAGE RankNTypes #-}

module Task7 where

import Lens.Micro
import System.FilePath.Posix
import Task5

-- Change the extension directory of all files to another (non-recursive).
changeExtensionFiles :: FS -> String -> FS
changeExtensionFiles fs ext =
  fs & dirContents . each . (filtered isFile) . name %~
  (`replaceExtensions` ext)
  where
    isFile :: FS -> Bool
    isFile fs' =
      case fs' ^? fileName of
        Just _  -> True
        Nothing -> False

-- Get the names of all files and directories recursively.
allResourcesNames :: FS -> [String]
allResourcesNames fs@(File _) = [fs ^. name]
allResourcesNames fs@(Dir _ _) =
  fs ^. name : (concatMap allResourcesNames $ fs ^. contents)

-- Delete the selected subdirectory only if it is empty.
deleteEmptyDirectory :: FS -> String -> FS
deleteEmptyDirectory fs path = fs & dirContents %~ (filter handleFS)
  where
    handleFS :: FS -> Bool
    handleFS fs'
      | (fs' ^. name /= path) = True
      | otherwise = not . null $ fs' ^. contents

-- Gets the path to the file keeping its relative path
move :: String -> Traversal' FS FS
move path f fs@(Dir name' _) =
  let result =
        case (fs ^? dirContents . each . filtered isNeed) of
          Just x  -> x & name %~ (name' </>)
          Nothing -> fs
   in (dirContents . each) f (fs {_contents = [result]})
  where
    isNeed :: FS -> Bool
    isNeed fs' =
      case fs' ^? name of
        Just x  -> x == path
        Nothing -> False
move _ _ fs = pure fs

-- Get the full path to the file with the file name relative to the FS start node.
getPath :: Traversal' FS String
getPath f fs@(Dir name' _) =
  (\val -> fs {_name = val}) <$> (f . addTrailingPathSeparator) name'
getPath f (File name') = File <$> f name'
