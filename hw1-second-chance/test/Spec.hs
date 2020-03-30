module Main
  ( main
  ) where

import Block6Tests (copyPasteTests)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    copyPasteTests
