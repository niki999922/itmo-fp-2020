{-# LANGUAGE InstanceSigs #-}

module Poli where

import Control.Monad
import Data.Monoid

private fun <T extends Object>lala(hmm: T): T {
    //TODO:lalal
}

data NPReader a b c d = NPReader { runReader1 :: a -> b, 
                               runReader2 :: b -> d, 
                               runReader2 :: a -> c 
}

instance  (Applicative NPReader e) => Monad (NPReader a b c) where
    return :: a -> NPReader e a
    return a = NPReader $ \_ -> a

    (>>=) :: NPReader e a -> (a -> NPReader e b) -> NPReader e b
    m >>= f = NPReader $ \r -> runReader (f $ runReader m r) r




Examples on Reader 
                   context   result
                   Reader r a = Reader {runReader :: (r -> a)}
someFunc :: Reader UserTable User
someFunc = do 
    la <- ask    --return all UserTable
    la <- asks (myFunc)    --return what you need   :: (r -> a) -> (Reader r a)   when you don't want read all USerTable
    local -- Модифицирует окружениие r (или UserTable) 
    return 'User'  -- results found user, because type is Reader a ne User

Examples on Writer
    Writer m a = Writer { runWriter :: (a, m) } -- где m это лог, а "a" результат

someF :: Writer String Int
someF = do
    tell "string" --add this string to log
    return 17

-- execWriter
-- runWriter

Examples on State
    State s a = State { runState :: s -> (a, s)}

someR :: State String Int
someR = do 
    get :: State s s  -- return state with ans == context
    put :: s -> State s () -- return State with () ans and s context
    modidfy f -- меняет context в State 


-- execState  state
-- evalState  result
-- runState


Examples on Transformers
someTTT :: ReaderT [String]
            (Writer String "`m`")
            `m`
            -- после вызова runReaderT вернётсся на самом деле Writer String `m`
            -- самого "`m`" не пишется, это я для понимания
            -- чтобы поменять внутреннюю монаду пишут
--lift $ lalala с внутренней монадой
