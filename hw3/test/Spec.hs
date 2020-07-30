module Main
  ( main
  ) where

import Task1Tests
import Task2Tests
import Task3Tests
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    geometry
    hashTableTests
    halyavaTests
