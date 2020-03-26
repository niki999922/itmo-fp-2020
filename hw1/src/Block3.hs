{-# LANGUAGE ScopedTypeVariables #-}

module Block3
  ( Name(..)
  , maybeConcat
  , eitherConcat
  , NonEmpty(..)
  , ThisOrThat(..)
  ) where

maybeConcat ::
     forall a f. (Foldable f, Monoid a)
  => f (Maybe a)
  -> a
maybeConcat = foldMap joinList
  where
    joinList :: Maybe a -> a
    joinList (Nothing) = mempty
    joinList (Just x)  = x

eitherConcat ::
     forall l f r. (Foldable f, Monoid l, Monoid r)
  => f (Either l r)
  -> (l, r)
eitherConcat = foldMap action
  where
    action :: Either l r -> (l, r)
    action (Left left)   = (left, mempty)
    action (Right right) = (mempty, right)

data NonEmpty a =
  a :| [a]
  deriving (Show, Eq)

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

instance Semigroup (ThisOrThat a b) where
  (<>) (This x) (That y)     = Both x y
  (<>) (That x) (This y)     = Both y x
  (<>) (This _) second       = second
  (<>) (That _) second       = second
  (<>) (Both _ y1) (This x2) = Both x2 y1
  (<>) (Both x1 _) (That x2) = Both x1 x2
  (<>) (Both _ _) second     = second

newtype Name =
  Name String
  deriving (Show, Eq)

instance Semigroup Name where
  (Name x) <> (Name y) = Name (x ++ "." ++ y)

instance Monoid Name where
  mempty = Name ""
