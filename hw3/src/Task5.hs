module Task5 where

import Control.Monad
import Lens.Micro
import System.Directory
import System.FilePath.Posix

data FS
  = Dir
      { _name     :: FilePath
      , _contents :: [FS]
      }
  | File
      { _name :: FilePath
      }
  deriving (Show)

name :: Lens' FS FilePath
name = lens _name (\obj val -> obj {_name = val})

contents :: Lens' FS [FS]
contents = lens _contents (\obj val -> obj {_contents = val})

dirName :: Traversal' FS FilePath
dirName f fs@(Dir x _) = (\val -> fs {_name = val}) <$> f x
dirName _ fs           = pure fs

dirContents :: Traversal' FS [FS]
dirContents f fs@(Dir _ x) = (\val -> fs {_contents = val}) <$> f x
dirContents _ fs           = pure fs

fileName :: Traversal' FS FilePath
fileName f (File x) = File <$> f x
fileName _ fs       = pure fs

-- "Scans" the given directory and creates an object of type FS like the getDirectory 'function.
getDirectory :: FilePath -> IO FS
getDirectory currentPath = do
  isFile <- doesFileExist currentPath
  case isFile of
    True -> return $ File (takeFileName currentPath)
    False -> do
      elements' <- listDirectory currentPath
      let elements = map (currentPath </>) elements'
      files <- filterM doesFileExist elements
      directories <- filterM doesDirectoryExist elements
      results <- mapM (\path -> getDirectory path) (files ++ directories)
      return $ Dir (takeFileName currentPath) results

-- A list of folder subtrees for Dir, otherwise an empty list
subDirs :: FS -> [FS]
subDirs = (^. contents)

-- Maybe with the name of the directory, if Dir, or Nothing otherwise.
getDirName :: FS -> Maybe String
getDirName = (^? dirName)

-- The name of the file if File, or an empty string otherwise.
getFileName :: FS -> Maybe String
getFileName = (^? fileName)

-- Change the name of the tree root to /.
renameRoot :: FS -> FS
renameRoot fs = fs & name .~ "/"

-- Add an arbitrary suffix to the root name of the tree.
addSuffix :: FS -> FS
addSuffix fs = fs & name %~ (++ "__some_suffix__")

-- Get the name of the very first folder in the list of subdirectories (Nothing, if there is none).
firstDirInSubDir :: FS -> Maybe String
firstDirInSubDir fs = fs ^. contents ^.. each . dirName ^? each

-- Get a list of only File names from Dir (non-recursively).
filesInDir :: FS -> [String]
filesInDir fs = fs ^. contents ^.. each . fileName
