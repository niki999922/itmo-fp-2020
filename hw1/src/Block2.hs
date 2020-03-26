{-# LANGUAGE ScopedTypeVariables #-}

module Block2
  ( splitOn
  , joinWith
  ) where

splitOn ::
     forall a. (Eq a)
  => a
  -> [a]
  -> [[a]]
splitOn spliter list = filter (not . null) $ foldr spliting [[]] list
  where
    spliting :: a -> [[a]] -> [[a]]
    spliting _ [] = [[]]
    spliting current arr@(x:xs)
      | current == spliter = [] : arr
      | otherwise = (current : x) : xs

joinWith ::
     forall a. (Eq a)
  => a
  -> [[a]]
  -> [a]
joinWith joiner = tail . foldl joinElements []
  where
    joinElements :: [a] -> [a] -> [a]
    joinElements x y = x ++ (joiner : y)
