module Block2Tests
  ( treeFoldableTests
  ) where

import Block1 (fromList)
import Block2 (joinWith, splitOn)
import Data.Foldable (toList)
import Data.List (sort)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property)

treeFoldableTests :: SpecWith ()
treeFoldableTests =
  describe "Testing Tree Foldable instance" $ do
    it "have to sort array [1,3,1,2] to [1,1,2,3]" $ do
      let array = [1, 3, 2, 1]
      (toList $ fromList array) `shouldBe` (sort array)
    it "property test for Foldable" $ do property checkSort
    it "testing joinWith x . splitOn x ≡ id" $ do
      let text = "yandex verstka egg"
      ((joinWith ' ' . splitOn ' ') text) `shouldBe` "yandex verstka egg"
    it "testing joinWith x . splitOn x ≡ id" $ do
      let text =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud"
      ((joinWith '-' . splitOn ' ') text) `shouldBe`
        "Lorem-ipsum-dolor-sit-amet,-consectetur-adipiscing-elit,-sed-do-eiusmod-tempor-incididunt-ut-labore-et-dolore-magna-aliqua.-Ut-enim-ad-minim-veniam,-quis-nostrud"
  where
    checkSort :: [Int] -> Bool
    checkSort []   = True
    checkSort list = (toList $ fromList list) == (sort list)
