module Task4
  ( fix
  , iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- iterate through one element as array
iterateElement :: a -> [a]
iterateElement = fix myIterate
  where
    myIterate :: (a -> [a]) -> a -> [a]
    myIterate f x = x : f x

-- fibonacci numbers
fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' :: (Integer -> Integer) -> Integer -> Integer
    fibonacci' f x
      | x == 0 = 1
      | x == 1 = 1
      | otherwise = f (x - 1) + f (x - 2)

-- factorial numbers
factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' f x
      | x == 0 = 1
      | x == 1 = 1
      | otherwise = f (x - 1) * x

-- map through using fix, like common map for array
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFix'
  where
    mapFix' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapFix' _ _ []     = []
    mapFix' f g (x:xs) = g x : f g xs
