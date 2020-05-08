{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import Debug.Trace
import Data.List
import qualified Data.Text as DT
import Data.Time.Clock
import System.Directory 
import System.FilePath.Posix
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Control.Monad
import Data.List.Split
-- import Filesystem ( removeTree )

import File
import Directory
import WorkingEnvironment
import Parser

    


-- import Options.Applicative
-- import Data.Semigroup ((<>))

-- data Sample = Sample { hello :: String}

-- sample :: Parser Sample
-- sample = Sample
      -- <$> strOption
          -- ( long "hello"
        --  <> metavar "TARGET"
        --  <> help "Target for the moving" )




main :: IO ()
main = undefined
-- main = greet =<< execParser opts
  -- where
    -- opts = info (sample <**> helper)
      -- ( fullDesc
    --  <> progDesc "Print a greeting for TARGET"
    --  <> header "hello - a test for optparse-applicative" )

-- greet :: Sample -> IO ()
-- greet (Sample h) = putStrLn $ "Hello, " ++ h
-- greet _ = return ()

mainq :: IO ()
mainq = do 
    putStrLn "Hello world"
    let path1 = "/Users/nikita/itmo-fp-2020/hw2/LICENSE"
    let path2 = "/Users/nikita/itmo-fp-2020/hw2/src"
    file1 <- readFile' path1
    state2 <- readDirectoriesState path2
    putStrLn $ fGetInformation file1
    putStrLn "\n___\n"
    putStrLn $ dGetInformation state2


-- myFileSystem -> StateT MyFileSystem (ExceptT String IO) ()

mainr :: IO WorkingEnvironment
mainr = do 
    -- putStrLn " "
    -- let path = "/Users/nikita/itmo-fp-2020/hw2/TEST_DIR/timeDir/BigHouse/flat1"
    -- createDirectory path
    let path = "/Users/nikita/itmo-fp-2020/hw2/TEST_DIR/timeDir"
    -- curTime <- getCurrentTime
    state <- readDirectoriesState path
    let workingState = WorkingEnvironment path [] state
    putStrLn "\n---------\nXXXXXXXXX\n---------\n"
    -- putStrLn " \n\n\n\nStart: \n"
    stateNew <- cycleRead workingState
    _ <- removeDirectoryRecursive $ weStartPath stateNew
    _ <- createDirectory $ weStartPath stateNew
    _ <- dSaveDirectory $ weWorkAround stateNew
    putStrLn "\n---------\nXXXXXXXXX\n---------\n"
    return stateNew
    -- print workingState
    -- putStrLn " "
    
    -- putStrLn $ show $ weWorkAround workingState
    -- putStrLn $ dGetInformation state
    -- let workingState2 = commandCD workingState "omg"
    -- print workingState2
    -- newWE <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
    -- newWE <- unpackStateExcept workingState (weCommandCD "BigHouse")
    -- putStrLn "__________________PeredddddddddDDDDDDDD__________________"
    -- print newWE
    -- newWE2 <- unpackStateExceptWithResult newWE $ weCommandLS "flat"
    -- print newWE
    -- newWE2 <- unpackStateExcept newWE (weCommandCreateFolder "!!!!!!!!NEW___1__FOLDER!!!!!")
    -- newWE3 <- unpackStateExcept newWE2 (weCommandCreateFolder "!!!!!!!!NEW___1__FOLDER!!!!!")
    -- unpackStateExceptWithResult newWE (weCommandInformation ".")

    -- putStrLn "__________________PosleeeeeeeeEEEEEEEee________2__________"
    -- print newWE2
    -- unpackStateExceptWithResult newWE (weCommandInformation "Flat.txt")
    -- newWE_1 <- unpackStateExcept newWE (weCommandCD "..")
    -- putStrLn "FINDIIIIIIIIIIIIIING FILEEEEEEEES___________"
    -- print newWE_1
    -- unpackStateExceptWithResult newWE_1 (weCommandFindFile "Flat.txt")
    -- unpackStateExceptWithResult newWE_1 (weCommandCat "Flat.txt")

    -- newWE2 <- unpackStateExcept newWE (weCommandCD "..")
    -- print newWE2
    -- putStrLn " "
    -- newWE3 <- unpackStateExcept newWE (weCommandCD "../..")
    -- print newWE3
    -- putStrLn " "

    -- newWE2 <- unpackStateExcept newWE (weCommandCD "flat2")
    -- print newWE2
    
    -- return ws1

-- writeFile path "text"

cycleRead :: WorkingEnvironment -> IO WorkingEnvironment
cycleRead we = do
  putStr $ (weGetCurrentDirectoryCMD we) ++ " > "
  command <- getLine
  case (runParser commandParser command) of
    Just x -> case x of 
        (CommandCD path, _) -> do
          newFs <- unpackStateExcept we $ weCommandCD path
          cycleRead newFs
        (CommandInformation path, _) -> do
          newFs <- unpackStateExceptWithResult we $ weCommandInformation path
          cycleRead newFs
        (CommandFindFile path, _) -> do
          newFs <- unpackStateExceptWithResult we $ weCommandFindFile path
          cycleRead newFs
        (CommandCat path, _) -> do
          newFs <- unpackStateExceptWithResult we $ weCommandCat path
          cycleRead newFs
        (CommandCreateFolder path, _) -> do
          newFs <- unpackStateExcept we $ weCommandCreateFolder path
          cycleRead newFs
        (CommandCreateFile path, _) -> do
          curTime <- getCurrentTime
          newFs <- unpackStateExcept we $ weCommandCreateFile path curTime
          cycleRead newFs
        (CommandRemove path, _) -> do
          newFs <- unpackStateExcept we $ weCommandRemove path
          cycleRead newFs
        (CommandWriteFile path content, _) -> do
          curTime <- getCurrentTime
          newFs <- unpackStateExcept we $ weCommandWriteFile path content curTime
          cycleRead newFs
        (CommandDir, _) -> do
          newFs <- unpackStateExceptWithResult we $ weCommandDir
          cycleRead newFs
        (CommandLS path, _) -> do
          newFs <- unpackStateExceptWithResult we $ weCommandLS path
          cycleRead newFs 
        (CommandCVSInit, _) -> do
          curTime <- getCurrentTime
          newFs <- unpackStateExcept we $ weCommandCVSInit curTime
          cycleRead newFs 
        (CommandCVSAdd path, _) -> do
          -- putStrLn "________________________Running CommandCVSAdd________________________"
          curTime <- getCurrentTime
          -- putStrLn "_____________________________________BEFORE_____________________________________"
          -- print we
          -- putStrLn "_____________________________________BEFORE_____________________________________"
          
          newFs <- unpackStateExcept we $ weCommandCVSAdd path curTime
          -- putStrLn "_____________________________________AFTER______________________________________"
          -- print newFs
          -- putStrLn "_____________________________________AFTER______________________________________"
          cycleRead newFs 
        (CommandCVSUpdate path text, _) -> do 
          curTime <- getCurrentTime
          newFs <- unpackStateExcept we $ weCommandCVSUpdate path text curTime
          cycleRead newFs   
        (CommandCVSHistory path, _) -> do 
          newFs <- unpackStateExceptWithResult we $ weCommandCVSHistory path
          cycleRead newFs   
        (CommandCVSCat path version, _) -> do 
          newFs <- unpackStateExceptWithResult we $ weCommandCVSCat path version
          cycleRead newFs
        (CommandCVSMergeRevs path version1 version2 strategy, _) -> do 
          curTime <- getCurrentTime
          newFs <- unpackStateExcept we $ weCommandCVSMergeRevs path version1 version2 strategy curTime
          cycleRead newFs   
        (CommandCVSDeleteVersion path version, _) -> do 
          newFs <- unpackStateExcept we $ weCommandCVSDeleteVersion path version
          cycleRead newFs   
        (CommandCVSRemove path, _) -> do 
          newFs <- unpackStateExcept we $ weCommandCVSRemove path
          cycleRead newFs   
        (CommandCVSShowEverything, _) -> do 
          newFs <- unpackStateExceptWithResult we $ weCommandCVSShowEverything
          cycleRead newFs   
        (CommandHelp, _) -> do 
          putStrLn helpText
          cycleRead we
        (CommandEmpty, _) -> cycleRead we
        (CommandExit, _) -> return we
    Nothing -> do
      putStrLn "Unknown command, for more information use \"help\""
      cycleRead we

-- cvs-cat BigHouse/flat1/Flat.txt
unpackStateExcept :: WorkingEnvironment -> StateWE WorkingEnvironment IO () -> IO WorkingEnvironment
unpackStateExcept we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left error -> do 
      putStrLn $ error
      return we
    Right eatherResNewFs -> return $ snd eatherResNewFs

unpackStateExceptWithResult :: WorkingEnvironment -> StateWE WorkingEnvironment IO String -> IO WorkingEnvironment
unpackStateExceptWithResult we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left error -> do 
      putStrLn $ error
      return we
    Right eatherResNewFs -> do
      putStrLn $ fst eatherResNewFs
      return $ snd eatherResNewFs


helpText :: String
helpText =  "cd <folder> -- перейти в директори\n" ++
            "dir -- показать содержимое текущей директори\n" ++
            "ls <folder> -- показать содержимое выбранной директори\n" ++
            "create-folder \"folder-name\" -- создать директорию в текуще\n" ++
            "cat <file> -- показать содержимое файл\n" ++
            "create-file \"file-name\" -- создать пустой файл в текущей директори\n" ++
            "remove <folder | file> -- удалить выборанную директорию или фай\n" ++
            "write-file <file> \"text\" -- записать текст в фай\n" ++
            "find-file \"file-name\" --  поиск файла в текущией директории и поддиректория\n" ++
            "information <file> -- показать информацию о файл\n" ++
            "information <folder> -- показать информацию о директори\n" ++
            "cvs-init -- инициализация СКВ в текущей выбранной директори\n" ++
            "cvs-add <file | folder> -- добавление файла или папки в СК\n" ++
            "cvs-update <file> \"comment\" -- добавление изменений файла в СК\n" ++
            "cvs-history <file> -- просмотр истории изменений файл\n" ++
            "cvs-cat <file> \"index\" -- просмотр конкретной ревизии файл\n" ++
            "cvs-merge-revs <file> \"index1\" \"index2\" \"left | right | both\" -\n" ++
            "объединение ревизий файла по заданным индексам, left, right, both или interactiv\n" ++
            "являются вариантами стратегий для обеъединени\n" ++
            "cvs-delete-version <file> \"index\" -- удалить заданную версию файла из ревизи\n" ++
            "cvs-remove <file> -- удалить файл из СК\n" ++
            "cvs-show-everything -- показать общую историю изменени\n" ++
            "help --  показать руководство по использовани\n" ++
            "exit -- завершение работы программы"