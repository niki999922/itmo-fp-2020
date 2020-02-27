module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg = \x f -> f x

-- ((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = f (Right negeate)
  where
    negeate = \x -> f (Left x)

-- Equivalent to double negation in ИИВ, so cannot deduce the type
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- Cannot deduce the type for 10 axioms in ИИВ
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- (((a -> Void) -> Void) -> Void) -> a -> Void
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f = f . doubleNeg
