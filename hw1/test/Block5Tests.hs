module Block5Tests
  ( expressionTests
  ) where

import Block5 (Expression (..), eval)
import Test.Hspec (SpecWith, describe, it, shouldBe)

expressionTests :: SpecWith ()
expressionTests =
  describe "Testing Expressions" $ do
    let five = Const 5
    let free = Const 3
    let seven = Const 7
    it "check 5 + 3 = 8" $ do (eval $ Addition five free) `shouldBe` (Right 8)
    it "check 1439 + 89752 = 91191" $ do
      (eval $ Addition (Const 1439) (Const 89752)) `shouldBe` (Right 91191)
    it "check 5 - 3 = 2" $ do
      (eval $ Subtraction five free) `shouldBe` (Right 2)
    it "check 5 * 3 = 15" $ do
      (eval $ Multiplication five free) `shouldBe` (Right 15)
    it "check 5 * 3 * 7 = 105" $ do
      (eval $ Multiplication (Multiplication five free) seven) `shouldBe`
        (Right 105)
    it "check 7 / 3 = 2" $ do (eval $ Division seven free) `shouldBe` (Right 2)
    it "check 7 / (3-3) = exception" $ do
      (eval $ Division seven (Subtraction free free)) `shouldBe`
        (Left "Divizion by zeroSubtraction (Const 3) (Const 3)")
    it "check 5 ^ 3 = 125" $ do (eval $ Power five free) `shouldBe` (Right 125)
    it "check 17 ^ 23 = 1444280090074984049" $ do
      (eval $ Power (Const 17) (Const 23)) `shouldBe`
        (Right 1444280090074984049)
    it "check 5 ^ -3 = exception" $ do
      (eval $ Power five (Negation free)) `shouldBe`
        (Left "Power on negate power =)Negation (Const 3)")
    it "check -3 = -3" $ do (eval $ Negation free) `shouldBe` (Right (-3))
