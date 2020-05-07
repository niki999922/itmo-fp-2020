{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Directory where

import File

import Debug.Trace

import Data.List
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Data.Time.Clock
import Data.Maybe

data Directory = Directory {
    dPath :: FilePath,
    dFiles :: [File],
    dSubDirectories :: [Directory],
    dPermissions :: Permissions
} deriving (Eq)

class Directorible d where
    dGetName :: d -> String
    dGetPath :: d -> FilePath
    dGetFiles :: d -> [File]
    dGetAmountFiles :: d -> Int
    dGetPermissions :: d -> Permissions
    dShowPermissons :: d -> String
    dGetSubDirectories :: d -> [Directory]
    dGetInformation :: d -> String
    dGetSizeDirectory :: d -> Int
    dGetFileByName :: d -> String -> File
    dFindFileByName :: d -> String -> Maybe File
    dAddSubDirectory :: d -> String -> d
    dAddFile :: d -> String -> UTCTime -> d
    dDeleteFile :: d -> String -> d
    dDeleteSubDirectory :: d -> String -> d
    dEditFileContext :: d -> String -> String -> UTCTime -> d
    dSaveDirectory :: d -> IO ()


instance Show Directory where
    show d = "\n\n" ++ (dPath d) ++ ":\n" ++ (show $ dFiles d) ++ "\n" ++ (show $ dSubDirectories d)

instance Directorible Directory where
    dGetName = takeFileName . dPath
    dGetPath = dPath
    dGetFiles = dFiles
    dGetAmountFiles = length . dGetFiles
    dGetSubDirectories = dSubDirectories
    dGetInformation = (fromMaybe "Something was bad duo to count information") . showDirectoryInformation
    dGetPermissions = dPermissions
    dGetSizeDirectory d = (sum (map (dGetSizeDirectory) (dGetSubDirectories d))) + (sum (map (fGetSize) (dGetFiles d)))
    dShowPermissons d = fromMaybe "Something was bad duo to gets permissons" $ do 
        let perm = dGetPermissions d
        Just ("\"" ++ isReadable perm ++ isWritable perm ++ isExecutable perm ++ isSearchable perm ++ "\"")
        where
            isReadable perm = if (readable perm) then "r" else ""
            isWritable perm = if (writable perm) then "w" else ""
            isExecutable perm = if (executable perm) then "e" else ""
            isSearchable perm = if (searchable perm) then "s" else ""
    dGetFileByName d name = head $ filter (\x -> (fGetName x) == name) (dGetFiles d) 
    dFindFileByName Directory{..} name = do 
        let files = filter (\x -> (fGetName x) == name) dFiles
        if (null files) 
            then do
                let res = filter isJust $ map (`dFindFileByName` name) dSubDirectories
                if (null res)
                    then 
                        Nothing
                    else 
                        head res
            else 
                Just $ head files
    dAddSubDirectory d@Directory{..} name = d {dSubDirectories = dSubDirectories ++ [Directory (dPath ++ "/" ++ name) [] [] allPermisson]}
    dAddFile d@Directory{..} name time = d {dFiles = dFiles ++ [File (dPath ++ "/" ++ name) "" allPermisson time True]}
    dDeleteFile d@Directory{..} name = d {dFiles = filter (\x -> (fGetName x) /= name) dFiles}
    dDeleteSubDirectory d@Directory{..} name = d {dSubDirectories = filter (\x -> (dGetName x) /= name) dSubDirectories}
    dEditFileContext d@Directory{..} name text time = do 
        let otherFiles = filter (\x -> (fGetName x) /= name) dFiles
        let editedFile = head $ filter (\x -> (fGetName x) == name) dFiles
        d {dFiles = otherFiles ++ [editedFile { fContent = text, fEditTime = time, fEdited = True}]}
    dSaveDirectory d@Directory{..} = do 
        mapM_ (fSaveFile) dFiles
        mapM_ (\x -> createDirectory $ dGetPath x) dSubDirectories 
        mapM_ (dSaveDirectory) dSubDirectories

    

readDirectoriesState :: FilePath -> IO Directory
readDirectoriesState currentPath = do 
    elements' <- listDirectory currentPath
    curDir <- getCurrentDirectory
    let elements = map (\x -> currentPath ++ "/" ++ x) elements'
    files <- filterM doesFileExist elements
    directories <- filterM doesDirectoryExist elements
    convertedFiles <- mapM (readFile') files
    convertedDirectories <- mapM (readDirectoriesState) directories
    permission <- getPermissions currentPath
    return $ Directory currentPath convertedFiles convertedDirectories permission

showDirectoryInformation :: Directory -> Maybe String 
showDirectoryInformation dir = do
    let path = dGetPath dir
    let permissions = dShowPermissons dir
    let sizeDirectory = dGetSizeDirectory dir
    let amountFilesInDirectory = dGetAmountFiles dir
    return ("Path: \"" ++ path ++
     "\"\nPermissions: " ++ permissions ++ 
     "\nSize: " ++ show sizeDirectory ++ " bytes"++
     "\nFiles in directory: " ++ show amountFilesInDirectory ++ " files\n")

allPermisson :: Permissions
allPermisson = (setOwnerSearchable True (setOwnerExecutable True (setOwnerWritable True (setOwnerReadable True emptyPermissions))))