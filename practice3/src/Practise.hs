{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE DeriveTraversable #-}  -- generates `traverse`
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}  --for 7 task!
-- {-# LANGUAGE TypeApplications       #-}   -- read @Int 7,        don't need me
-- {-# LANGUAGE DeriveFoldable    #-}  -- generates `foldr` and `foldMap`
module Practise
  ( main'
  ) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal as CI
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List
import qualified Data.Text as T
import GHC.Generics
import GHC.Word (Word8)
import System.Environment
import System.IO
import Text.Read

strToWord8s :: String -> [Word8]
strToWord8s = CI.unpackBytes . C8.pack

data TaskState
  = Open
  | InProgress
  | Done
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Task =
  Task
    { state :: TaskState
    , name  :: String
    }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Organaizer =
  Organaizer
    { tasks :: [Task]
    }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

addTask :: Organaizer -> String -> Organaizer
addTask organaizer = Organaizer . (: (tasks organaizer)) . (Task Open)

deleteTask :: Organaizer -> String -> Organaizer
deleteTask organaizer context =
  Organaizer $ filter ((context /=) . name) (tasks organaizer)

changeStatus :: Organaizer -> String -> TaskState -> Organaizer
changeStatus organaizer context status =
  Organaizer
    ((map
        (\(Task _ x) -> Task status x)
        (filter ((context ==) . name) (tasks organaizer))) ++
     filter ((context /=) . name) (tasks organaizer))

showTasks :: Organaizer -> TaskState -> [Task]
showTasks organaizer status = filter ((status ==) . state) (tasks organaizer)

-- serializeTasks (Organaizer [Task Open "kek", Task Done "che kak dela"])
serializeTasks :: FilePath -> Organaizer -> IO ()
serializeTasks file = writeFile file . LC8.unpack . encode

deserializeTasks :: FilePath -> IO (Maybe Organaizer)
deserializeTasks file = do
  withFile file ReadMode $ \f -> do
    str <- hGetLine f
    let desStr = decode (B.pack $ strToWord8s str) :: Maybe Organaizer
    return desStr

--decode (B.pack $ strToWord8s "{\"nameq\":\"Joe\",\"age\":12}") :: Maybe Person
fakeMain :: IO ()
fakeMain
    -- let task1 = Task Open "it's taks 1"
    -- let task2 = Task InProgress "it's taks 2"
    -- let task3 = Task Done "it's taks 3"
    -- let organaizer = Organaizer [task1, task2, task3]
    -- let updateOrganaizer = addTask organaizer "New added task"
    -- putStr $ encode toJSON updateOrganaizer
    -- serializeTasks "./myTestFile2.json" updateOrganaizer
    -- putStr $ encode toJSON updateOrganaizer
    -- org <- deserializeTasks "./myTestFile1.json"
    -- putStrLn $ show org
 = do
  return ()

main' :: IO ()
main' = do
  args <- getArgs :: IO [String]
  let action = (!!) args 0
  let text = (!!) args 1
  actionHandler action text args
    -- print args

actionHandler :: String -> String -> [String] -> IO ()
actionHandler action text args
  | action == "add-task" = do
    organaizer <- deserializeTasks "./myTestFile2.json"
    case organaizer of
      Just x -> do
        serializeTasks "./myTestFile2.json" (addTask x text)
      Nothing ->
        serializeTasks "./myTestFile2.json" (addTask (Organaizer []) text)
  | action == "delete-task" = do
    organaizer <- deserializeTasks "./myTestFile2.json"
    case organaizer of
      Just x -> do
        serializeTasks "./myTestFile2.json" (deleteTask x text)
      Nothing -> serializeTasks "./myTestFile2.json" (Organaizer [])
  | action == "change-status" = do
    organaizer <- deserializeTasks "./myTestFile2.json"
    case organaizer of
      Just x -> do
        serializeTasks
          "./myTestFile2.json"
          (changeStatus x text (stringToStatus ((!!) args 2)))
      Nothing -> serializeTasks "./myTestFile2.json" (Organaizer [])
  | action == "show-only" = do
    organaizer <- deserializeTasks "./myTestFile2.json"
    case organaizer of
      Just x  -> putStrLn $ show $ showTasks x (stringToStatus text)
      Nothing -> return ()
  | otherwise = return ()

stringToStatus :: String -> TaskState
stringToStatus "done"        = Done
stringToStatus "in-progress" = InProgress
stringToStatus _             = Open

{-
Задание 2
Задание про работу с файлами и аргументами командной строки. Рекомендуется использовать optparse-applicative для парсинга аргументов командной строки.

Реализовать простую утилиту для поддержания TODO-листа, она должна уметь:

Добавить таску
Изменить статус таски (open, in progress, done)
Удалить таску
Посмотреть на все таски с каким-либо статусом.
Например:

stack exec -- todo-list  --add-task 'Придумать задания для практики'
-}