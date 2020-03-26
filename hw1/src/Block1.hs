{-# LANGUAGE ScopedTypeVariables #-}

module Block1
  ( nextDay
  , isWeekend
  , daysToParty
  , afterDays
  , isEmptyTree
  , fromNat
  , toNatural
  , isEven
  , divNat
  , modNat
  , findFromTree
  , amountTree
  , fromList
  , insertTree
  , deleteFromTree
  , DayWeek(..)
  , Nat(..)
  ) where

import Tree (Tree (..))

instance Eq DayWeek where
  x == y = show x == show y

data DayWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

nextDay :: DayWeek -> DayWeek
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

isWeekend :: DayWeek -> Bool
isWeekend Monday    = False
isWeekend Tuesday   = False
isWeekend Wednesday = False
isWeekend Thursday  = False
isWeekend Friday    = False
isWeekend Saturday  = True
isWeekend Sunday    = True

daysToParty :: DayWeek -> Integer
daysToParty Saturday  = 6
daysToParty Sunday    = 5
daysToParty Monday    = 4
daysToParty Tuesday   = 3
daysToParty Wednesday = 2
daysToParty Thursday  = 1
daysToParty Friday    = 0

afterDays :: DayWeek -> Integer -> DayWeek
afterDays day n
  | n >= 7 = afterDays day (n `mod` 7)
  | n > 0 = afterDays (nextDay day) (n - 1)
  | otherwise = day

data Nat
  = Zero
  | Super Nat

join :: Nat -> Nat
join Zero      = Zero
join (Super x) = x

toNatural :: Integer -> Nat
toNatural x
  | x > 0 = Super (toNatural (x - 1))
  | otherwise = Zero

fromNat :: Nat -> Integer
fromNat (Super x) = (+ 1) $ fromNat x
fromNat (Zero)    = 0

instance Show Nat where
  show x = show (showNumber x 0)
    where
      showNumber :: Nat -> Integer -> Integer
      showNumber (Super xs) num = showNumber xs (num + 1)
      showNumber (Zero) num     = num

instance Num Nat where
  (+) (Zero) (Zero)       = Zero
  (+) (Super x) (Zero)    = Super x
  (+) (Zero) (Super y)    = Super y
  (+) (Super x) (Super y) = Super (Super (x + y))
  (-) (Zero) (Zero)       = Zero
  (-) (Super x) (Zero)    = Super x
  (-) (Zero) (Super _)    = Zero
  (-) (Super x) (Super y) = x - y
  (*) (Zero) (Zero)       = Zero
  (*) (Super _) (Zero)    = Zero
  (*) (Zero) (Super _)    = Zero
  (*) (Super x) (Super y) = (Super x) + (Super x) * y
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Eq Nat where
  (==) Zero Zero           = True
  (==) Zero (Super _)      = False
  (==) (Super _) Zero      = False
  (==) (Super x) (Super y) = (join x) == (join y)

instance Ord Nat where
  (<=) Zero Zero           = True
  (<=) Zero (Super _)      = True
  (<=) (Super _) Zero      = False
  (<=) (Super x) (Super y) = (join x) <= (join y)

isEven :: Nat -> Bool
isEven x = fromNat x `mod` 2 == 0

divNat :: Nat -> Nat -> Nat
divNat x y = toNatural $ (fromNat x) `div` (fromNat y)

modNat :: Nat -> Nat -> Nat
modNat x y = toNatural $ (fromNat x) `mod` (fromNat y)

isEmptyTree :: (Tree a) -> Bool
isEmptyTree Leaf = True
isEmptyTree _    = False

amountTree :: (Tree a) -> Int
amountTree (Node nodes left right) =
  (amountTree left) + (amountTree right) + (length nodes)
amountTree (Leaf) = 0

isLess :: Int -> [Int] -> Bool
isLess x (y:_) = x < y
isLess _ []    = False

findFromTree :: (Tree Int) -> Int -> (Tree Int)
findFromTree tree = findNumber tree
  where
    findNumber Leaf _ = Leaf
    findNumber (Node nodes left right) number
      | number `elem` nodes = tree
      | isLess number nodes = findFromTree left number
      | otherwise = findFromTree right number

insertTree :: (Tree Int) -> Int -> (Tree Int)
insertTree Leaf x = Node [x] Leaf Leaf
insertTree (Node nodes left right) number = insertFunc
  where
    insertFunc
      | number `elem` nodes = Node (number : nodes) left right
      | number `isLess` nodes =
        let newLeft = insertTree left number
         in Node nodes newLeft right
      | otherwise =
        let newRight = insertTree right number
         in Node nodes left newRight

deleteFromTree :: (Tree Int) -> Int -> (Tree Int)
deleteFromTree Leaf _ = Leaf
deleteFromTree (Node nodes left right) number
  | number `elem` nodes =
    case (init nodes) of
      [] -> createNewRight right nums_
        where getNumbers :: (Tree Int) -> [Int]
              getNumbers Leaf = []
              getNumbers (Node nodes2 left2 right2) =
                nodes2 <> (getNumbers left2) <> (getNumbers right2)
              nums_ :: [Int]
              nums_ = getNumbers left
              createNewRight :: (Tree Int) -> [Int] -> (Tree Int)
              createNewRight _ []        = Leaf
              createNewRight tree (x:xs) = createNewRight (insertTree tree x) xs
      _x:_xs -> Node (init nodes) left right
  | number `isLess` nodes =
    let newLeft = deleteFromTree left number
     in Node nodes newLeft right
  | otherwise =
    let newRight = deleteFromTree right number
     in Node nodes left newRight

fromList :: [Int] -> (Tree Int)
fromList [] = Leaf
fromList list@(_x:_xs) = fromListImpl Leaf list
  where
    fromListImpl :: (Tree Int) -> [Int] -> (Tree Int)
    fromListImpl tree []     = tree
    fromListImpl tree (x:xs) = fromListImpl (insertTree tree x) xs
