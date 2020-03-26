{-# LANGUAGE InstanceSigs #-}

module Tree
  ( Tree(..)
  ) where

data Tree a
  = Leaf
  | Node
      { nodeF  :: [a]
      , leftF  :: (Tree a)
      , rightF :: (Tree a)
      }
  deriving (Show)

instance (Eq a) => Eq (Tree a) where
  (==) Leaf Leaf = True
  (==) Leaf Node {} = False
  (==) Node {} Leaf = False
  (==) (Node nodesx lx rx) (Node nodesy ly ry) =
    (head nodesx == (head nodesy)) && (lx == ly) && (rx == ry)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node elems left right) =
    (foldMap f left) <> (f $ head elems) <> (foldMap f right)
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node elems left right) =
    let rightFold = foldr f z right
        middle = foldr f rightFold elems
        leftFold = foldr f middle left
     in leftFold
