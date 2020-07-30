module Task2Tests(hashTableTests) where

import Control.Concurrent.Async (mapConcurrently_, replicateConcurrently_)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad.List (forM_)
import Control.Monad.STM (atomically)
import Criterion.Main
import Task2
import Test.Hspec (SpecWith, describe, it, shouldBe)



sizeCHTVector :: ConcurrentHashTable k v -> IO Int
sizeCHTVector table = atomically $ do
  (HashTable vec) <- readTVar $ hashTable table
  return $ length vec

putElements :: [Int] -> ConcurrentHashTable Int Int -> IO ()
putElements elements table = forM_ elements $ \el -> putCHT el el table

putElements' :: ConcurrentHashTable Int Int -> IO ()
putElements' table = do
    let elements = [1..1000000]
    forM_ elements $ \el -> do
        putCHT el el table

hashTableTests :: SpecWith ()
hashTableTests =
  describe "Testing concurrent hash table" $ do
    it "Test newCHT" $ do
      table <- newCHT
      result <- sizeCHTVector table
      result `shouldBe` baseSize
    it "Test sizeCHT empty" $ do
      table <- newCHT
      result <- sizeCHT table
      result `shouldBe` 0
    it "Test sizeCHT" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 1 1 table
      result <- sizeCHT table
      result `shouldBe` 1
    it "Test getCHT on empty" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      result <- getCHT 1 table
      result `shouldBe` Nothing
    it "Test getCHT on non-empty" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 1 1 table
      result <- getCHT 1 table
      result `shouldBe` Just 1
    it "Test putCHT" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 10 20 table
      result <- getCHT 10 table
      result `shouldBe` Just 20
    it "Test putCHT with expand" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 1 21 table
      putCHT 2 22 table
      putCHT 3 23 table
      putCHT 4 24 table
      putCHT 5 25 table
      putCHT 6 26 table
      putCHT 7 27 table
      putCHT 8 28 table
      putCHT 9 29 table
      result <- getCHT 9 table
      result `shouldBe` Just 29
    it "Test performance putCHT" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      defaultMain [
        bgroup "hash table" [ bench "put table 10^6 operations"  $ whnf putElements' table]
        ]
    it "Test cuncurrent performance putCHT" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      defaultMain [
        bgroup "hash table" [ bench "put table 100 cuncurrent threads"  $ nfIO $ mapConcurrently_ (\el -> putElements [1..el] table) [1..100]]
        ]
    it "Test cuncurrent working" $ do
      table <- newCHT :: IO (ConcurrentHashTable Int Int)
      replicateConcurrently_ 5 $ putElements [1..100] table
      result <- sizeCHT table
      result `shouldBe` (100 :: Int)
