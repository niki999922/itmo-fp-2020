module Block3Tests
  ( datasFoldableTests
  ) where

import Block3 (Name (..), NonEmpty (..), ThisOrThat (..), eitherConcat, maybeConcat)
import Test.Hspec (SpecWith, describe, it, shouldBe)

datasFoldableTests :: SpecWith ()
datasFoldableTests =
  describe "Testing Block 3" $ do
    it "maybeConcat [Just [1,2,3], Nothing, Just [4,5]] ==  [1,2,3,4,5]" $ do
      (maybeConcat [Just [(1 :: Integer), 2, 3], Nothing, Just [4, 5]]) `shouldBe`
        [(1 :: Integer), 2, 3, 4, 5]
    it
      "eitherConcat [Left [3], Right [1,2,3], Left [5], Right [4,5]] ==  ([3,5], [1,2,3,4,5])" $ do
      (eitherConcat
         [ Left [(3 :: Integer)]
         , Right [(1 :: Integer), 2, 3]
         , Left [5]
         , Right [4, 5]
         ]) `shouldBe`
        ([(3 :: Integer), 5], [(1 :: Integer), 2, 3, 4, 5])
    it "instance Semigroup NonEmpty" $ do
      (((2 :: Integer) :| [3, 4, 5]) <> (0 :| [1])) `shouldBe`
        ((2 :: Integer) :| [3, 4, 5, 0, 1])
    it "instance Semigroup ThisOrThat" $ do
      (This "a" <> That (3 :: Integer)) `shouldBe` (Both "a" (3 :: Integer))
    it "instance Semigroup Name" $ do
      (Name "root" <> Name "server") `shouldBe` (Name "root.server")
    it "instance Monoid Name" $ do (mempty) `shouldBe` (Name "")
