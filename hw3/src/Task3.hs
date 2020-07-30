{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Task3 where

import Control.Monad.ST
import Data.STRef

class (Ord var, Show var) => Variable var

instance Variable Int
instance Variable Double
instance Variable Bool
instance Variable String

infixr 4 @=
infixr 4 @>
infixr 4 @>=
infixr 4 @<
infixr 4 @<=
infixr 4 @==
infixr 4 @/=

infixr 4 @@>
infixr 4 @@>=
infixr 4 @@<
infixr 4 @@<=
infixr 4 @@==
infixr 4 @@/=

infixr 0 #

class HalyavaScript script s where
  (@=)          :: Variable a => STRef s a -> script s a -> script s ()

  (@>=)         :: Variable a => STRef s a -> script s a -> script s Bool
  (@>)          :: Variable a => STRef s a -> script s a -> script s Bool
  (@<)          :: Variable a => STRef s a -> script s a -> script s Bool
  (@<=)         :: Variable a => STRef s a -> script s a -> script s Bool
  (@==)         :: Variable a => STRef s a -> script s a -> script s Bool
  (@/=)         :: Variable a => STRef s a -> script s a -> script s Bool

  (@@>=)        :: Variable a => STRef s a -> STRef s a -> script s Bool
  (@@<)         :: Variable a => STRef s a -> STRef s a -> script s Bool
  (@@>)         :: Variable a => STRef s a -> STRef s a -> script s Bool
  (@@<=)        :: Variable a => STRef s a -> STRef s a -> script s Bool
  (@@==)        :: Variable a => STRef s a -> STRef s a -> script s Bool
  (@@/=)        :: Variable a => STRef s a -> STRef s a -> script s Bool

  (#)           :: script s a -> script s b -> script s b

  sWithVar      :: Variable a => a -> (STRef s a -> script s b) -> script s b
  sWorkWithVar  :: Variable a => STRef s a -> (a -> script s b) -> script s b
  sReadVar      :: Variable a => STRef s a -> script s a

  sFun          :: (Variable a, Variable b) => b -> (STRef s a -> STRef s b -> script s c) -> a -> script s b
  sIf           :: script s Bool -> script s a -> script s a -> script s a
  sWhile        :: script s Bool -> script s a -> script s ()

  sWrap         :: Variable a => a -> script s a


instance HalyavaScript ST s where
  (@=) stRef val = do
    val' <- val
    writeSTRef stRef val'

  (@>) stRef val = do
    val' <- val
    (> val') <$> readSTRef stRef
  (@>=) stRef val = do
    val' <- val
    (>= val') <$> readSTRef stRef
  (@<) stRef val = do
    val' <- val
    (< val') <$> readSTRef stRef
  (@<=) stRef val =do
    val' <- val
    (<= val') <$> readSTRef stRef
  (@==) stRef val =do
    val' <- val
    (== val') <$> readSTRef stRef
  (@/=) stRef val =do
    val' <- val
    (/= val') <$> readSTRef stRef

  (@@>) stRef1 stRef2 = (>) <$> readSTRef stRef1 <*> readSTRef stRef2
  (@@>=) stRef1 stRef2 = (>=) <$> readSTRef stRef1 <*> readSTRef stRef2
  (@@<) stRef1 stRef2 = (<) <$> readSTRef stRef1 <*> readSTRef stRef2
  (@@<=) stRef1 stRef2 = (<=) <$> readSTRef stRef1 <*> readSTRef stRef2
  (@@==) stRef1 stRef2 = (==) <$> readSTRef stRef1 <*> readSTRef stRef2
  (@@/=) stRef1 stRef2 = (/=) <$> readSTRef stRef1 <*> readSTRef stRef2

  (#) = (>>)

  sWithVar initVal f = do
    stRef <- newSTRef initVal
    f stRef

  sWorkWithVar stRef f = do
    val <- readSTRef stRef
    f val

  sReadVar stRef = do
    readSTRef stRef

  sWhile condition clojure = do
    isTrue <- condition
    case (isTrue) of
      True -> do
          _ <- clojure
          sWhile condition clojure
      False -> return ()

  sIf condition ifClojure elseClojure = do
    isTrue <- condition
    if (isTrue)
    then
      ifClojure
    else
      elseClojure

  sFun result script firstArgument = do
    firstArgument' <- newSTRef firstArgument
    result' <- newSTRef result
    _ <- script firstArgument' result'
    readSTRef result'

  sWrap = pure


runScript :: (forall s. ST s a) -> a
runScript = runST
