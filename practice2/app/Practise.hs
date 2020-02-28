module Main
  ( main
  , smartReplicate
  , contains
  , FName
  ) where

import Data.Char (isDigit, toLower)

-- Задание 1
-- Функция должна повторять каждый элемент столько раз, чему равен сам элемент.
-- ghci> smartReplicate [1,2,3]
-- [1,2,2,3,3,3]
smartReplicate :: Integral a => [a] -> [a]
smartReplicate = walk
  where
    listOf :: Integral a => a -> a -> [a]
    listOf x n
      | n <= 0 = []
      | otherwise = x : listOf x (n - 1)
    walk :: Integral a => [a] -> [a]
    walk [] = []
    walk (x:xs) = listOf x x ++ walk xs

-- Задание 2
-- Напишите функцию, которой передаётся список списков и некоторый элемент. Эта функция должна вернуть список только тех списков, которые содержат переданный элемент.
-- ghci> contains 3 [[1..5], [2,0], [3,4]]
-- [[1,2,3,4,5],[3,4]]
-- True if contain x, else otherwise
contains :: Integral a => a -> [[a]] -> [[a]]
contains x list = init $ customFilter (checkOnContain x) list
  where
    checkOnContain :: Integral a => a -> [a] -> Bool
    checkOnContain x list = not $ null $ filter (\t -> t == x) list
    customFilter :: Integral a => ([a] -> Bool) -> [[a]] -> [[a]]
    customFilter _ [] = [[]]
    customFilter p (x:xs)
      | p x = [x] ++ customFilter p xs
      | otherwise = customFilter p xs

{-
Task 2

  Implement `Ord` instance for newtype
  below.

  It should follow the described semantics:

  1) If both strings being compared start
  from a digit symbol (`0..9`), read
  numeric prefixes and compare strings by
  these prefixes. If both strings start
  from the same number (e.g. `01aba` and
  `1caba`), comparison is performed by
  rest of string characters
  case-insensitive
  2) Otherwise, compare two strings
  case-insensitive

  instance Show (BinTree a) where
    show t = ... -- Тут идет объявление вашей функции

  Minimal complete definition for Ord
  compare | (<=)
-}
newtype FName =
  FName String
  deriving (Eq, Read, Show)

instance Ord FName where
  (>) (FName x) (FName y) = y < x
  (<=) (FName x) (FName y) = not $ y < x
  (>=) (FName x) (FName y) = not $ x < y
  min (FName x) (FName y) =
    if (x <= y)
      then (FName x)
      else (FName y)
  max (FName x) (FName y) =
    if (x >= y)
      then (FName x)
      else (FName y)
  compare (FName x) (FName y)
    | x < y = LT
    | x > y = GT
    | otherwise = EQ
  (<) (FName x) (FName y) = compareString intPrefixFirst intPrefixSecond
    where
      numberPrefix :: String -> String
      numberPrefix [] = []
      numberPrefix (x:xs)
        | isDigit x = x : numberPrefix xs
        | otherwise = []
      convertToInt :: String -> Integer
      convertToInt str = read str :: Integer
      intPrefixFirst :: Integer
      intPrefixFirst = convertToInt ("0" ++ numberPrefix x)
      intPrefixSecond :: Integer
      intPrefixSecond = convertToInt ("0" ++ numberPrefix y)
      compareString str1 str2 =
        case (compare str1 str2) of
          LT -> True
          GT -> False
          EQ -> compatesuffix
            where firstSuffix :: String
                  firstSuffix = drop (length $ numberPrefix x) x
                  secondSuffix :: String
                  secondSuffix = drop (length $ numberPrefix y) y
                  compatesuffix :: Bool
                  compatesuffix =
                    map toLower firstSuffix < map toLower secondSuffix

-------------------------------------------
-- Task 3
--
-- When launched from ghci, following
-- results will be printed.
-- Explain the difference in calls
-- (why one call returns while other
-- goes into infinite loop):
--
-- > sumAndLog [8] (Box 2)
-- Just 3.0
--
-- > sumAndLog [8 -10] loop
-- Nothing

-- > sumAndLogD [8] (BoxD 2)
-- Just 3.0
--
-- > sumAndLogD [8 -10] loop
-- {.. infitinte loop ..}
--
-- return MayBe
--
--------------------------------------------
newtype Box a = Box a -- not recursive and can have one constr
data BoxD a = BoxD a -- recursive and can have BoxD a, BoxD BoxD a, BoxD BoxD BoxD a ...
-- it's rofl compiler, потому что будет бес конечно распоковывать loop и не посчитает даже сумму?

sumAndLog as (Box base) =
  let s = sum as
   in if s < 0
        then Nothing
        else Just (log s / log base)

sumAndLogD as (BoxD base) =
  let s = sum as
   in if s < 0
        then Nothing
        else Just (log s / log base)

loop = loop

main = pure ()
