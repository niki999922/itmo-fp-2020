module Task2 where

import Control.Concurrent.STM.TMVar (isEmptyTMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.STM (STM, atomically)
import qualified Data.Hashable as DH (Hashable, hash)
import Data.Vector ((!))
import qualified Data.Vector as V

type Element k v = TVar (Maybe (k, v))

newtype HashTable k v =
  HashTable (V.Vector (Element k v))

data ConcurrentHashTable k v =
  ConcurrentHashTable
    { hashTable     :: TVar (HashTable k v)
    , fullnessTable :: TVar Int
    }

baseSize :: Int
baseSize = 8

expandConst :: Int
expandConst = 30

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT table = atomically $ do readTVar $ fullnessTable table

newCHT :: IO (ConcurrentHashTable k v)
newCHT =
  atomically $ do
    emptyVector <- HashTable <$> V.replicateM baseSize (newTVar Nothing)
    vector' <- newTVar emptyVector
    emptyFullness <- newTVar 0
    return $ ConcurrentHashTable vector' emptyFullness

-- return element from key, or first free element by this key
getElement ::
     (DH.Hashable k, Eq k) => k -> V.Vector (Element k v) -> STM (Element k v)
getElement key vector = do
  let vectorSize = V.length vector
  let hash = (DH.hash key `mod` vectorSize)
  let indexList = [hash .. vectorSize - 1] ++ [0 .. hash - 1]
  result <- newEmptyTMVar
  forM_ indexList $ \ind -> do
    let element = vector ! ind
    element' <- readTVar element
    case element' of
      Just (key', _) -> when (key == key') $ putTMVar result element
      Nothing -> do
        isEmpty <- isEmptyTMVar result
        when isEmpty $ putTMVar result element
  takeTMVar result

getCHT :: (DH.Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable t _) =
  atomically $ do
    (HashTable table) <- readTVar t
    element <- getElement key table
    element' <- readTVar element
    case element' of
      Just (_, value) -> return $ Just value
      Nothing         -> return Nothing

putCHT :: (DH.Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable t s) =
  atomically $ do
    (HashTable table) <- readTVar t
    let tableSize = length table
    fullnessTable' <- readTVar s
    element <- getElement key table
    unboxedElement <- readTVar element
    case (unboxedElement) of
      Nothing -> writeTVar s (fullnessTable' + 1)
      Just _  -> return ()
    writeTVar element (Just (key, value))
    when (fullnessTable' + 1 == tableSize) $ do
      newVector <- expandTable table fullnessTable'
      writeTVar t newVector
  where
    expandTable ::
         (DH.Hashable k, Eq k)
      => (V.Vector (Element k v))
      -> Int
      -> STM (HashTable k v)
    expandTable table curSize = do
      ht@(HashTable emptyVector) <-
        HashTable <$> V.replicateM (curSize + expandConst) (newTVar Nothing)
      V.forM_ table $ \el -> do
        val <- readTVar el
        case val of
          Just (x, _) -> do
            element' <- getElement x emptyVector
            writeTVar element' val
          Nothing -> return ()
      return ht
