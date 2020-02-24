{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

-- Заселить следующие типы (формально undefined тоже считается заселением типа (причем любого), но в решении ожидается что-то отличное от undefined):

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity x = (Left x, Left x)

associator :: (a, (b, c)) -> ((a, b), c)
associator = undefined

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = undefined
