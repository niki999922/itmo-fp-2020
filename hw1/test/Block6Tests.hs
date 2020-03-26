module Block6Tests
  ( copyPasteTests
  ) where

import Block6 (Parser (..), element, eof, ok, satisfy, stream)
import Test.Hspec (SpecWith, describe, it, shouldBe)

copyPasteTests :: SpecWith ()
copyPasteTests =
  describe "Testing ok, eof, satisfy, element, stream for Parse" $ do
    let text = "11133 text 2 mama"
    it "testing ok apply all" $ do
      (runParser ok text) `shouldBe` (Just ((), "11133 text 2 mama"))
    it "testing eof Nothing" $ do (runParser eof text) `shouldBe` (Nothing)
    it "testing eof Just" $ do (runParser eof "") `shouldBe` (Just ((), ""))
    it "testing satisfy" $ do
      (runParser (satisfy (const True)) text) `shouldBe`
        (Just ('1', "1133 text 2 mama"))
    it "testing element" $ do
      (runParser (element '1') text) `shouldBe` (Just ('1', "1133 text 2 mama"))
    it "testing stream Nothing" $ do
      (runParser (stream "3") text) `shouldBe` (Nothing)
    it "testing stream Just" $ do
      (runParser (stream "1") text) `shouldBe` (Just ("1", "1133 text 2 mama"))
