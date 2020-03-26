module Block1Tests
  ( daysTests
  , natTests
  , treeTests
  ) where

import Block1 (DayWeek (..), Nat (..), afterDays, amountTree, daysToParty, deleteFromTree, divNat,
               findFromTree, fromList, fromNat, insertTree, isEmptyTree, isEven, isWeekend, modNat,
               nextDay, toNatural)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Tree (Tree (..))

daysTests :: SpecWith ()
daysTests =
  describe "Testing Days" $ do
    it "nextDay of Monday Tuesday" $ do (nextDay Monday) `shouldBe` (Tuesday)
    it "afterDays of Monday on 8 Tuesday" $ do
      (afterDays Monday 8) `shouldBe` Tuesday
    it "isWeekend Monday false" $ do (isWeekend Monday) `shouldBe` (False)
    it "isWeekend Sunday true" $ do (isWeekend Sunday) `shouldBe` (True)
    it "daysToParty from Monday is 4" $ do (daysToParty Monday) `shouldBe` (4)

natTests :: SpecWith ()
natTests =
  describe "Testing Nat" $ do
    let two = Super (Super Zero)
    let six = two + two + two
    it "2 + 6 = 8" $ do (show $ two + six) `shouldBe` "8"
    it "2 * 6 = 12" $ do (show $ two * six) `shouldBe` "12"
    it "6 - 2 = 4" $ do (show $ six - two) `shouldBe` "4"
    it "6 == 6" $ do six `shouldBe` six
    it "2 < 6 = true" $ do (two < six) `shouldBe` True
    it "2 < 6 = true" $ do (two < six) `shouldBe` True
    it "even 2 = true" $ do (isEven two) `shouldBe` True
    it "6 mod 2 = 3" $ do (show $ six `modNat` two) `shouldBe` "0"
    it "6 div 2 = 0" $ do (show $ six `divNat` two) `shouldBe` "3"
    it "toNatural from 2 to Nat 2" $ do (toNatural 2) `shouldBe` two
    it "fromNat from Nat 2 to 2" $ do (fromNat two) `shouldBe` 2

treeTests :: SpecWith ()
treeTests =
  describe "Testing Tree" $ do
    let testTree =
          Node
            [5, 5, 5]
            (Node [4] (Node [1, 1, 1] Leaf Leaf) Leaf)
            (Node [6] Leaf Leaf)
    it "isEmptyTree test" $ do (isEmptyTree testTree) `shouldBe` False
    it "Count size tree = 8" $ do (amountTree testTree) `shouldBe` 8
    it "Find subtree [6]" $ do
      (findFromTree testTree 6) `shouldBe` (Node [6] Leaf Leaf)
    it "Add new element subtree [6]" $ do
      let tmpTree = insertTree testTree 6
      (rightF tmpTree) `shouldBe` (Node [6, 6] Leaf Leaf)
    it "Crete tree from list [1,2] " $ do
      let tmpTree = fromList [1, 2]
      (tmpTree) `shouldBe` (Node [1] Leaf (Node [2] Leaf Leaf))
    it "Delete element 6" $ do
      let tmpTree = deleteFromTree testTree 6
      (tmpTree) `shouldBe`
        (Node [5, 5, 5] (Node [4] (Node [1, 1, 1] Leaf Leaf) Leaf) Leaf)
