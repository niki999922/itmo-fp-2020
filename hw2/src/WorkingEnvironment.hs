{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module WorkingEnvironment where

import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Data.Time.Clock
import Data.List.Split



import Directory
import File

data WorkingEnvironment = WorkingEnvironment {
    weStartPath :: FilePath,
    weCurrentPath :: [FilePath],
    weWorkAround :: Directory
} deriving (Eq)

instance Show WorkingEnvironment where
    show WorkingEnvironment {..} = "Current working dir: \"" ++ intercalate "/" weCurrentPath ++ "\"\nHere workAround________" ++ show weWorkAround

class WorkingEnvironmentable t where
    weCommandCD :: FilePath -> StateT t (ExceptT String IO) ()
    weCommandInformation :: FilePath -> StateT t (ExceptT String IO) String
    weCommandFindFile :: String -> StateT t (ExceptT String IO) String
    weCommandCat :: String -> StateT t (ExceptT String IO) String
    weCommandCreateFolder :: String -> StateT t (ExceptT String IO) ()
    weCommandCreateFile :: String -> UTCTime -> StateT t (ExceptT String IO) ()
    weCommandRemove :: FilePath -> StateT t (ExceptT String IO) ()
    weCommandWriteFile :: FilePath -> String -> UTCTime -> StateT t (ExceptT String IO) ()
    weCommandDir :: StateT t (ExceptT String IO) String
    weCommandLS :: FilePath -> StateT t (ExceptT String IO) String

    weGetCurrentDirectoryCMD :: t -> String
    weSaveWorkingEnvironment :: StateT t (ExceptT String IO) ()
    -- weIsFileExist :: 
    -- weIsDirectoryExist ::


instance WorkingEnvironmentable WorkingEnvironment where
    weGetCurrentDirectoryCMD WorkingEnvironment {..} = weStartPath ++ (if (null weCurrentPath) then "" else "/" ++ (intercalate "/" weCurrentPath))
    weCommandCD dir = do
            we@(WorkingEnvironment {..}) <- get
            let pathList = splitOn "/" dir 
            handleCases pathList
            return ()
        where
            handleCases :: [FilePath] -> StateT WorkingEnvironment (ExceptT String IO) ()        
            handleCases pathList 
                | onlyDots pathList = do 
                    we@(WorkingEnvironment {..}) <- get
                    if (length pathList > length weCurrentPath)
                        then 
                            lift $ throwE $ "Don't more than maximum path " ++ (show $ length pathList) ++ " > " ++ (show $ length weCurrentPath)
                        else
                            modify (\s -> s { weCurrentPath = take ((length weCurrentPath) - (length pathList)) weCurrentPath})
                | onlyNotDots pathList = do 
                    we@(WorkingEnvironment {..}) <- get
                    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
                    let exprectedWorkDirectory = weGetCurrentWorkAround (trace ("currentWorkDirectory = " ++ dGetPath currentWorkDirectory) currentWorkDirectory) pathList
                    case exprectedWorkDirectory of
                        Just _ -> modify (\s -> s { weCurrentPath = (weCurrentPath ++ pathList) })
                        Nothing -> lift $ throwE $ "Can't find directory: \"" ++ dir ++ "\""
                | otherwise  = do 
                    lift $ throwE $ "Don't use multypath with \"..\", only \"..\" or without \"..\" in: \"" ++ show pathList ++ "\""

            onlyDots :: [FilePath] -> Bool
            onlyDots list = (length (filter (\x -> x == "..") list )) == (length list)
            onlyNotDots :: [FilePath] -> Bool
            onlyNotDots list = (length (filter (\x -> x /= "..") list )) == (length list)
                
    weCommandInformation path = do
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            let pathList = splitOn "/" path
            if (fromMaybe False $ isDirectory currentWorkDirectory pathList) 
                then do
                    let (Just dir) = weGetCurrentWorkAround currentWorkDirectory pathList
                    return $ dGetInformation dir
                else
                    if (fromMaybe False $ isFile currentWorkDirectory pathList) 
                        then do
                            let (Just dir) = weGetCurrentWorkAround currentWorkDirectory (init pathList)
                            let file = dGetFileByName dir (last pathList)
                            return $ fGetInformation file
                        else do
                            if (((length pathList == 1) && (head pathList == "")) || ((length pathList == 1) && (head pathList == ".")))
                                then do 
                                    return $ dGetInformation currentWorkDirectory
                                else do 
                                    lift $ throwE $ "Can't find file or directory with this path: \"" ++ (show pathList) ++ "\""
                                    return ""
    
    weCommandFindFile name = do 
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            case (dFindFileByName currentWorkDirectory name) of
                Just x -> return $ "File found, path is: " ++ fPath x
                Nothing -> do
                    lift $ throwE $ "Can't find file with name: \"" ++ name ++ "\""
                    return ""

    weCommandCat name = do 
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            case (dFindFileByName currentWorkDirectory name) of
                Just x -> return $ "File found, content is: " ++ fContent x
                Nothing -> do
                    lift $ throwE $ "Can't find file with name: \"" ++ name ++ "\""
                    return ""

    weCommandCreateFolder name = do 
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            case (find (\x -> (dGetName x) == name) (dSubDirectories currentWorkDirectory)) of 
                Just x -> lift $ throwE $ "Can't create folder, this name is taken \"" ++ (dGetPath x) ++ "\""
                Nothing -> do
                    modify (\s -> s {weWorkAround = (recurseConstructDirectory weWorkAround weCurrentPath name)})    

    weCommandCreateFile name currentTime = do 
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            case (find (\x -> (fGetName x) == name) (dFiles currentWorkDirectory)) of 
                Just x -> lift $ throwE $ "Can't create file, this name is taken \"" ++ (fGetPath x) ++ "\""
                Nothing -> do
                    modify (\s -> s {weWorkAround = (recurseConstructFile weWorkAround weCurrentPath name currentTime)})                    

    weCommandRemove path = do
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            let pathList = splitOn "/" path
            if (fromMaybe False $ isDirectory currentWorkDirectory pathList) 
                then do 
                    let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
                    modify (\s -> s {weWorkAround = recurseDeleteDirectory weWorkAround leftPart rightPart}) 
                else
                    if (fromMaybe False $ isFile currentWorkDirectory pathList) 
                        then do 
                            let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
                            modify (\s -> s {weWorkAround = recurseDeleteFile weWorkAround leftPart rightPart}) 
                        else lift $ throwE $ "Can't find file or directory for removing with this path: \"" ++ (show pathList) ++ "\""

    weCommandWriteFile path text time = do
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            let pathList = splitOn "/" path
            if (fromMaybe False $ isFile currentWorkDirectory pathList) 
                then do 
                    let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
                    modify (\s -> s {weWorkAround = recurseEditFileContext weWorkAround leftPart rightPart text time}) 
                else lift $ throwE $ "Can't find file for edit context with this path: \"" ++ (show pathList) ++ "\""

    weCommandDir = do 
            we@(WorkingEnvironment {..}) <- get
            let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            return $ intercalate "\n" $ (map dGetName $ dSubDirectories currentWorkDirectory) ++ (map fGetName $ dFiles currentWorkDirectory)

    weCommandLS path = do 
            we@(WorkingEnvironment {..}) <- get
            let pathList = splitOn "/" path
            case (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ pathList)) of 
                Just needDirectory -> do 
                    return $ intercalate "\n" $ (map dGetName $ dSubDirectories needDirectory) ++ (map fGetName $ dFiles needDirectory)
                Nothing ->
                    lift $ throwE $ "Can't find directory for printing condition with this path: \"" ++ (show pathList) ++ "\""
    weSaveWorkingEnvironment = undefined
            -- we@(WorkingEnvironment {..}) <- get
            -- we
            -- let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
            

weGetCurrentWorkAround :: Directory -> [FilePath] -> Maybe Directory
weGetCurrentWorkAround startDir path = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Maybe Directory
        recurseGo Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            if (null sort) 
                then 
                    Nothing
                else
                    recurseGo (head sort) xs
        recurseGo dir [] = Just dir

recurseConstructDirectory :: Directory -> [FilePath] -> String -> Directory
recurseConstructDirectory startDir path name = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Directory
        recurseGo d@Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
            d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort) xs)]}
        recurseGo dir [] = dAddSubDirectory dir name

recurseDeleteDirectory :: Directory -> [FilePath] -> String -> Directory
recurseDeleteDirectory startDir path name = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Directory
        recurseGo d@Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
            d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort) xs)]}
        recurseGo dir [] = dDeleteSubDirectory dir name


recurseConstructFile :: Directory -> [FilePath] -> String -> UTCTime -> Directory
recurseConstructFile startDir path name time = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Directory
        recurseGo d@Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
            d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort) xs)]}
        recurseGo dir [] = dAddFile dir name time

recurseDeleteFile :: Directory -> [FilePath] -> String -> Directory
recurseDeleteFile startDir path name = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Directory
        recurseGo d@Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
            d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort) xs)]}
        recurseGo dir [] = dDeleteFile dir name

recurseEditFileContext :: Directory -> [FilePath] -> String -> String -> UTCTime -> Directory
recurseEditFileContext startDir path name text time = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Directory
        recurseGo d@Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
            d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort) xs)]}
        recurseGo dir [] = dEditFileContext dir name text time


isDirectory :: Directory -> [FilePath] -> Maybe Bool
isDirectory startDir path = recurseGo startDir path
    where
        recurseGo ::  Directory -> [FilePath] -> Maybe Bool
        recurseGo Directory{..} (x:xs) = do 
            let sort = filter (\p -> x == (dGetName p)) dSubDirectories
            if (null sort) 
                then 
                    Just False
                else
                    recurseGo (head sort) xs
        recurseGo dir [] = Just True

isFile :: Directory -> [FilePath] -> Maybe Bool
isFile startDir path = do 
        let endPath = last path
        case (weGetCurrentWorkAround startDir (init path)) of 
            Just (Directory {..}) -> do
                if (length (filter (\f -> (fGetName f) == endPath) dFiles) > 0) 
                then 
                    Just True 
                else
                    Just False   
            Nothing -> Just False

correctList :: [String] -> ([String], String)
correctList list
    | (length list == 1) = ([], head list)
    | (length list >= 2) = (init list, last list)
    | otherwise = ([], "")