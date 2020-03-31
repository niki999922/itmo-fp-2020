module Block6Tests
  ( copyPasteTests
  ) where

import Block6 (Parser (..), element, eof, numberParser, ok, pspParser, satisfy, stream)
import Control.Applicative (Alternative (..))
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
    it "testing many element" $ do
      (runParser (many $ element '1') text) `shouldBe`
        (Just ("111", "33 text 2 mama"))
    it "testing parsing psp" $ do
      (runParser pspParser "(())()") `shouldBe` (Just ((), ""))
    it "testing parsing broken psp" $ do
      (runParser pspParser ")()(") `shouldBe` (Nothing)
    it "testing parsing number \"+123 a\"" $ do
      (runParser numberParser "+123 a") `shouldBe` (Just ("+123", " a"))
    it "testing parsing number \"-123 a\"" $ do
      (runParser numberParser "-123 a") `shouldBe` (Just ("-123", " a"))
    it "testing parsing number \"123 a\"" $ do
      (runParser numberParser "123 a") `shouldBe` (Just ("+123", " a"))
    it "testing parsing number \"00123 a\"" $ do
      (runParser numberParser "00123 a") `shouldBe` (Just ("+00123", " a"))
