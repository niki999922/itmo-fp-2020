module Task5
  ( zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

-- Church zero
zero :: Nat a
zero _ x = x

-- Church increment
succChurch :: Nat a -> Nat a
succChurch n f x = f $ n f x

-- Plus two Church numbers
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus x y f s = x f $ y f s

-- Mult two Church numbers
churchMult :: Nat a -> Nat a -> Nat a
churchMult x y f = x $ y f

-- Convert Church number to Int
churchToInt :: Nat Integer -> Integer
churchToInt x = x (+ 1) 0
