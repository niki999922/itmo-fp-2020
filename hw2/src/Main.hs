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
    let path = "/Users/nikita/itmo-fp-2020/hw2/timeDir"
    -- curTime <- getCurrentTime
    state <- readDirectoriesState path
    let workingState = WorkingEnvironment path [] state
    putStrLn " \n\n\n\nStart: \n"
    stateNew <- cycleRead workingState
    putStrLn "\n---------\n\n\n\n"
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
        (CommandHelp, _) -> do 
          putStrLn "Please write help all in ALL, NIKITA!!!"
          cycleRead we
        (CommandExit, _) -> return we
        
    Nothing -> do
      putStrLn "Unknown command, HERE ----HELP"
      cycleRead we


unpackStateExcept :: WorkingEnvironment -> StateT WorkingEnvironment (ExceptT String IO) () -> IO WorkingEnvironment
unpackStateExcept we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left error -> do 
      putStrLn $ error
      return we
    Right eatherResNewFs -> return $ snd eatherResNewFs

unpackStateExceptWithResult :: WorkingEnvironment -> StateT WorkingEnvironment (ExceptT String IO) String -> IO WorkingEnvironment
unpackStateExceptWithResult we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left error -> do 
      putStrLn $ error
      return we
    Right eatherResNewFs -> do
      putStrLn $ fst eatherResNewFs
      return $ snd eatherResNewFs