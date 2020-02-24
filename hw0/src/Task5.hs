module Task5
  ( zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

-- Как вы знаете из курса по теории типов, в лямбда-исчисления можно кодировать натуральные числа и арифметические операции над ними.
-- Определим тип нумералов Черча следующим образом:
type Nat a = (a -> a) -> a -> a

-- Определим нуль:
zero :: Nat a
zero f x = x

-- Определить функцию-последователя (он же инкремент):
succChurch :: Nat a -> Nat a
succChurch = undefined

-- Реализовать арифметические операции (сложение и умножение):
churchPlus, churchMult :: Nat a -> Nat a -> Nat a
churchPlus = undefined

churchMult = undefined

-- Реализовать функцию, которая по нумералу Черча сопоставляет обычное целое число:
churchToInt :: Nat Integer -> Integer
churchToInt = undefined
-- Данная функция должна удовлетворять следующим равенствам:
-- churchToInt zero = 0
-- churchToInt (succChurch number) = 1 + churchToInt number
-- churchToInt (churchPlus m n) = churchToInt m + churchToInt n
-- churchToInt (churchMult m n) = churchToInt m * churchToInt n
