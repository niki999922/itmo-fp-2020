module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

-- Предположим у нас есть функция:
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- Реализовать с помощью s и const следующие функции:
composition :: (b -> c) -> (a -> b) -> a -> c
composition f g = \x -> f $ g x

-- (должна вести себя аналогично оператору (.) из стандартной библиотеки) ||| типо написал
identity :: a -> a
identity = undefined

-- (должна вести себя аналогично тождественной функции id, определенной в Prelude)
contraction :: (a -> a -> b) -> a -> b
contraction = undefined

permutation :: (a -> b -> c) -> b -> a -> c
permutation = undefined
