module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

-- Заселить среди данных типов те, которые возможно.

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

pierce :: ((a -> b) -> a) -> a
pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined
