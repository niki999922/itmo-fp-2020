module Block4Tests
  ( sumMaybeTests
  ) where

import Block4 (stringSum)
import Test.Hspec (SpecWith, describe, it, shouldBe)

sumMaybeTests :: SpecWith ()
sumMaybeTests =
  describe "Testing sum Maybe Int" $ do
    it "have to be Just 10" $ do
      (stringSum " 1 2 3 4") `shouldBe` (Just (10 :: Integer))
    it "have to be Just 18298" $ do
      (stringSum " 1 2 3 4 123 9      9399 399            34 8324") `shouldBe`
        (Just (18298 :: Integer))
    it "have to be Nothing" $ do
      (stringSum " 1 2 kekekekekek kekek 3 4") `shouldBe`
        (Nothing :: Maybe Integer)
