module Task4
  ( fix
  , iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

-- В модуле Data.Function определена функция fix, которая является аналогом комбинатора неподвижной точки:
fix :: (a -> a) -> a
-- Реализовать с помощью fix следующие функции:
iterateElement :: a -> [a]
iterateElement = undefined

-- Данная функция должна удовлетворять равенству: iterateElement x == [x, x..]
fibonacci :: Integer -> Integer
fibonacci = undefined

factorial :: Integer -> Integer
factorial = undefined

mapFix :: (a -> b) -> [a] -> [b]
mapFix = undefined
