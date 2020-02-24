module Main where

-------------------------
-- Practice 1
-------------------------
------------------------------------------
-- Implement sorting of number sequences
--
-- You should implement three different
-- sorting algorithms.
--
-- Provide implementation in `solve` function.
-- Use `words` function to split whole input to words
-- and `readInt` to convert a word to an integer.
--
-- Your program will be given a sequence
-- of integers (written to stdin).
-- First integer takes value from range [0..2] and
-- signifies sorting algorithm to use.
-- Rest of integers is a sequence that is to be sorted.
--
-- You can compile and run your program with
-- `cabal run practice1`.
--
-------------------------------
main = interact solve

--printMStr :: String
--printMStr = print "Hello"
bSort' :: (Ord a) => [a] -> [a]
bSort' [x] =           [x]
bSort' (x1:x2:xs)
  | (x2 < x1) = x2 : (bSort' (x1 : xs))
  | otherwise = x1 : (bSort' (x2 : xs))

bSort :: (Ord a) => [a] -> [a]
bSort x =
  if ((bSort' x) == x) then x else bSort $ bSort' x

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

solve :: String -> String
solve s = "Hello " ++ s ++ "!"
