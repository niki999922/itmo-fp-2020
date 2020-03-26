module Main
  ( main
  ) where

import Block1Tests (daysTests, natTests, treeTests)
import Block2Tests (treeFoldableTests)
import Block3Tests (datasFoldableTests)
import Block4Tests (sumMaybeTests)
import Block5Tests (expressionTests)
import Block6Tests (copyPasteTests)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    daysTests
    natTests
    treeTests
    treeFoldableTests
    datasFoldableTests
    sumMaybeTests
    expressionTests
    copyPasteTests
