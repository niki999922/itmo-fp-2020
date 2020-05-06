module MyExamples where

import System.Directory
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans (lift)


returnReader :: Reader String String -> Reader String String
returnReader r = r  

changeLocal :: Reader String String
changeLocal = do
    context1 <- ask
--    changes <- RJ.local (\e -> e ++ "TTT") returnReader
    context2 <- ask

    return $ context1 ++ " ______ " ++ context2
someFunc :: IO ()
someFunc = putStrLn "someFunc2"

testWR :: WriterT String
          (Reader String)
          String
testWR = do
    tell "my State 1"
    lift $ asks (\i -> " __" ++ i ++ " wow__ ")
    -- return "res"

lalaal :: (Monad f, Monad g, Monad r) => f (g (r a)) -> a
lalaal = undefined     
