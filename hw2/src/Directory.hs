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
    dPermissions :: Permissions,
    dVCSSubDirectories :: [Directory]
} deriving (Eq)

class Directorible d where
    dGetName :: d -> String
    dGetPath :: d -> FilePath
    dGetFiles :: d -> [File]
    dGetAmountFiles :: d -> Int
    dGetPermissions :: d -> Permissions
    dShowPermissons :: d -> String
    dGetSubDirectories :: d -> [Directory]
    dGetVCSSubDirectories :: d -> [Directory]
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
    dCVSAddFile :: d -> String -> UTCTime -> d
    dCVSAddDirectory :: d -> UTCTime -> d
    dCreateCVSDir :: d -> String -> UTCTime -> d
    dCVSUpdate :: d -> String -> String -> UTCTime -> d
    dFindCVSDirSub :: d -> String -> Maybe d
    dCVSLastNumberOfCommit :: d -> Int
    dCVSDeleteVersion :: d -> String -> String -> d
    dCVSRemove :: d -> String -> d
    dGetAllFilesPathes  :: d -> [FilePath]
    


instance Show Directory where
    show d = "\n\n" ++ (dPath d) ++ ":\n" ++ (show $ dFiles d) ++ "\nVCS_SubDir: " ++ (show $ dVCSSubDirectories d) ++ "\nSubDir: " ++ (show $ dSubDirectories d) 

instance Directorible Directory where
    dGetName = takeFileName . dPath
    dGetPath = dPath
    dGetFiles = dFiles
    dGetAmountFiles = length . dGetFiles
    dGetSubDirectories = dSubDirectories
    dGetVCSSubDirectories = dVCSSubDirectories
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
    dAddSubDirectory d@Directory{..} name = d {dSubDirectories = dSubDirectories ++ [Directory (dPath ++ "/" ++ name) [] [] allPermisson []]}
    dAddFile d@Directory{..} name time = d {dFiles = dFiles ++ [File (dPath ++ "/" ++ name) "" allPermisson time True]}
    dDeleteFile d@Directory{..} name = d {dFiles = filter (\x -> (fGetName x) /= name) dFiles}
    dDeleteSubDirectory d@Directory{..} name = d {dSubDirectories = filter (\x -> (dGetName x) /= name) dSubDirectories}
    dEditFileContext d@Directory{..} name text time = do 
        let otherFiles = filter (\x -> (fGetName x) /= name) dFiles
        let editedFile = head $ filter (\x -> (fGetName x) == name) dFiles
        d {dFiles = otherFiles ++ [editedFile { fContent = text, fEditTime = time, fEdited = True}]}
    dSaveDirectory d@Directory{..} = do 
        mapM_ fSaveFile dFiles
        mapM_ (\x -> createDirectory $ dGetPath x) dSubDirectories 
        -- putStrLn $ "aaaaaaaaaaaaaaa_______1__1___\nwe are here: " ++ dPath ++ "\n" ++ show dSubDirectories
        -- putStrLn $ "aaaaaaaaaaaaaaa_______1__2___\nwe are here: " ++ dPath ++ "\n" ++ show dVCSSubDirectories
        mapM_ (\x -> createDirectory $ dGetPath x) dVCSSubDirectories 
        -- putStrLn "aaaaaaaaaaaaaaa_______2________aaaaaaaaaaaaaaaaa"
        -- cvs-add BigHouse/flat1/Flat.txt
        -- cvs-add BigHouse/Flat.txt
        mapM_ (dSaveDirectory) dSubDirectories
        mapM_ (dSaveDirectory) dVCSSubDirectories
    dCVSAddFile d@Directory{..} name time = do 
        let expectedDir = filter (\x -> (dGetName x) == (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
        -- if (trace ("_________________HHHHHHHEEEEEEERRRREEEEEEE_______") null expectedDir)
        if (null expectedDir)
        then 
            dCreateCVSDir d name time
            -- d {dVCSSubDirectories = dVCSSubDirectories ++ [dCreateCVSDir d name time]}
        else 
            d
    dCreateCVSDir d@Directory{..} name time = do 
        -- cvs-add BigHouse/flat1/Flat.txt
        -- cvs-add BigHouse/Flat.txt
        -- let needFile =  head $ filter (\x -> (fGetName x) == name) (trace ("TRY FILTERRRRRRR_______:::::" ++ (show $ filter (\x -> (fGetName x) == name) dFiles)) dFiles)
        let needFile =  head $ filter (\x -> (fGetName x) == name) dFiles
        let copyFile = needFile {fPath = (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0/" ++ name) }
        let commitFile = File (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0/commit") "initial" allPermisson time True
        let subCVSDir = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0") [copyFile, commitFile] [] allPermisson []
        let newCVSDIr = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name))) [] [subCVSDir] allPermisson []
        -- d {dVCSSubDirectories = dVCSSubDirectories ++ [trace (dPath ++ "/" ++ (".VCS_____-__----___--__--____---_" ++ (takeFileName name))) newCVSDIr]}    
        -- d {dVCSSubDirectories = (trace ("HMMMMMMMMM--->\n" ++ printTree 0 "" (dVCSSubDirectories ++ [newCVSDIr]) ++ "\n<-MMMMMMMMMH") $ dVCSSubDirectories ++ [newCVSDIr])}    
        let ans = d {dVCSSubDirectories =  dVCSSubDirectories ++ [newCVSDIr]}
        -- trace ("HMMMMMMMMM--->\n" ++ printTree 0 "" (trace ("NAMEEEEEEEE_____: " ++ (show  ans)) ans) ++ "\n<-MMMMMMMMMH") ans    
        ans    

    dCVSAddDirectory d@Directory{..} time = do 
        let filesNames = map fGetName dFiles
        let newDirState = foldl' (\x y -> dCVSAddFile x y time) d filesNames
        let newSubDirState = map (`dCVSAddDirectory` time) $ dGetSubDirectories newDirState
        newDirState {dSubDirectories = newSubDirState}

    dCVSUpdate d@Directory{..} name text time = do 
        let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
        let (Just cvsDir) = dFindCVSDirSub d name 
        let maxNumber = dCVSLastNumberOfCommit cvsDir

        let needFile =  head $ filter (\x -> (fGetName x) == name) dFiles
        let copyFile = needFile {fPath = (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1) ++ "/" ++ name) }
        let commitFile = File (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1) ++ "/commit") text allPermisson time True
        let subCVSDir = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1)) [copyFile, commitFile] [] allPermisson []
        let updatetCVS = cvsDir { dSubDirectories = (dGetSubDirectories cvsDir) ++ [subCVSDir]}
        let ans = d {dVCSSubDirectories = otherCvs ++ [updatetCVS]}
        -- trace ("HMMMMMMMMM--->\n" ++ printTree 0 "" ans ++ "\n<-MMMMMMMMMH") ans    
        ans
    dCVSDeleteVersion d@Directory{..} name version = do 
        let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
        let (Just cvsDir) = dFindCVSDirSub d name 
        -- let maxNumber = dCVSLastNumberOfCommit cvsDir
        let newSubDirectoriesCVS = filter (\x -> (dGetName x) /= version) (dGetSubDirectories cvsDir)

        let updatetCVS = cvsDir { dSubDirectories = newSubDirectoriesCVS}
        let ans = d {dVCSSubDirectories = otherCvs ++ [updatetCVS]}
        -- trace ("HMMMMMMMMM--->\n" ++ printTree 0 "" ans ++ "\n<-MMMMMMMMMH") ans    
        ans
    dCVSRemove d@Directory{..} name = do 
        let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
        let ans = d {dVCSSubDirectories = otherCvs}
        -- trace ("HMMMMMMMMM--->\n" ++ printTree 0 "" ans ++ "\n<-MMMMMMMMMH") ans    
        ans

    dFindCVSDirSub d@Directory{..} name = do 
        let expectedDir = filter (\x -> (dGetName x) == (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
        if (null expectedDir)
        then 
            Nothing 
        else 
            Just $ head expectedDir

    dCVSLastNumberOfCommit d@Directory{..} = maximum $ map (\x -> read (dGetName x) :: Int) dSubDirectories
    dGetAllFilesPathes d@Directory{..} = (map fGetPath dFiles) ++ (concatMap dGetAllFilesPathes dSubDirectories) 

readDirectoriesState :: FilePath -> IO Directory
readDirectoriesState currentPath = do 
    elements' <- listDirectory currentPath
    curDir <- getCurrentDirectory
    let elements = map (\x -> currentPath ++ "/" ++ x) elements'
    files <- filterM doesFileExist elements
    directories <- filterM doesDirectoryExist elements
    convertedFiles <- mapM (readFile') files
    let cvsDirs = filter (\x -> "." `isPrefixOf`(takeFileName x)) directories
    let justDirs = filter (\x -> not ("." `isPrefixOf`(takeFileName x))) directories
    convertedJustDirectories <- mapM (readDirectoriesState) justDirs
    convertedCVSDirectories <- mapM (readDirectoriesState) cvsDirs
    permission <- getPermissions currentPath
    return $ Directory currentPath convertedFiles convertedJustDirectories permission convertedCVSDirectories

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


printTree :: Int -> String -> Directory -> String 
printTree depth nameDir d@Directory {..} = (intercalate "\n" $ map (\x -> generateTabs depth nameDir ++ show x) $ map (takeFileName) $ map (fPath) dFiles) ++ showdSubDirectories ++ showdVCSSubDirectories
    where 
        generateTabs :: Int -> String -> String
        generateTabs n nameDir = (concat $ replicate n "|---") ++ nameDir ++ "> "
        showdVCSSubDirectories = do 
            if (not $ null dVCSSubDirectories)
            then
                "\n" ++ (intercalate "\n" $ map (\x -> printTree (depth + 1) ("v_" ++ dGetName x) x) dVCSSubDirectories)
            else ""    
        showdSubDirectories = do 
            if (not $ null dSubDirectories)
            then
                "\n" ++(intercalate "\n" $ map (\x -> printTree (depth + 1) ("s_" ++dGetName x) x) dSubDirectories)
            else ""    
            