module Task3Tests(halyavaTests) where

import Task3
import Test.Hspec (SpecWith, describe, it, shouldBe)


halyavaTests :: SpecWith ()
halyavaTests =
  describe "Testing HalyavaScript" $ do
    it "Test log2 of 14 = 4" $ do
      (runScript $ log2 14) `shouldBe` 4
    it "Test log2 of 16 = 4" $ do
      (runScript $ log2 16) `shouldBe` 4
    it "Test log2 of 18 = 5" $ do
      (runScript $ log2 18) `shouldBe` 5
    it "Test log2 of 908 = 9" $ do
      (runScript $ log2 908) `shouldBe` 10
    it "Test duplicateStr 12345" $ do
      (runScript $ duplicateStr "12345") `shouldBe` "1234512345"
    it "Test chooseName Sasuke" $ do
      (runScript $ chooseName 0) `shouldBe` "Hello Sasuke"

-- | For a given @x@ calculates @ceiling (log2 (a))@
log2 :: HalyavaScript script s => Int -> script s Int
log2 =
  sFun (0 :: Int) $ \a logCnt ->
  sWithVar (0 :: Int) $ \accum ->
    accum @= (sWrap 1) #
    logCnt @= (sWrap 0) #
    sWhile (a @@> accum)
      ( accum @= (sWorkWithVar accum $ \acc -> sWrap $ acc + acc) #
        logCnt @= (sWorkWithVar logCnt $ \l -> sWrap $ l + 1)
      )

duplicateStr :: HalyavaScript script s => String -> script s String
duplicateStr =
  sFun "init" $ \v0 res ->
    res @= (sWorkWithVar v0 $ \el -> sWrap $ el ++ el)

chooseName :: HalyavaScript script s => Int -> script s String
chooseName =
  sFun ("" :: String) $ \_ res ->
  sWithVar ("Sasuke" :: String) $ \vNameDefault ->
  sWithVar ("Name" :: String) $ \vName ->
  sWithVar (1 :: Int) $ \v1 ->
  sWhile (v1 @> (sWrap 0))
    ( v1 @= (sWorkWithVar v1 $ \val -> sWrap $ val - 1)) #
  sWithVar (True :: Bool) $ \v2 ->
  sWithVar (False :: Bool) $ \v3 ->
  sIf (v2 @@== v3)
    ( vName @= (sWrap "Naruto"))
    ( vName @= (sReadVar vNameDefault)) #
  res @= (sWorkWithVar vName $ \name -> sWrap $ "Hello " ++ name)
