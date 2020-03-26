{-# LANGUAGE ScopedTypeVariables #-}

module Block4
  ( stringSum
  ) where

import Block2 (splitOn)
import Text.Read (readMaybe)

stringSum :: (Read a, Num a) => String -> Maybe a
stringSum text = fmap sum $ traverse readMaybe $ splitOn ' ' text

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf x)            = Leaf $ f x
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure = Leaf
  (<*>) (Leaf f) val            = f <$> val
  (<*>) (Branch left right) val = Branch (left <*> val) (right <*> val)

instance Foldable Tree where
  foldr f z (Leaf a)            = f a z
  foldr f z (Branch left right) = foldr f (foldr f z right) left

instance Traversable Tree where
  traverse f (Leaf value) = Leaf <$> f value
  traverse f (Branch left right) =
    Branch <$> traverse f left <*> traverse f right

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (<*>) (lf :| ls) (x :| xs) = lf x :| (fmap lf xs ++ (ls <*> (x : xs)))

instance Foldable NonEmpty where
  foldr f z (x :| [])       = f x z
  foldr f z (x :| (xs:xss)) = f x (foldr f z (xs :| xss))

instance Monad NonEmpty where
  (>>=) (x :| xs) f =
    let (l :| ls) = f x
     in l :| (ls ++ (xs >>= (unwrap . f)))
    where
      unwrap :: NonEmpty a -> [a]
      unwrap (_x :| _xs) = _x : _xs

instance Traversable NonEmpty where
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
