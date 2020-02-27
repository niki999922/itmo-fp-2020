{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (leftEitherAssoc, rightEitherAssoc)

leftEitherAssoc :: Either a (Either b c) -> Either (Either a b) c
leftEitherAssoc (Left a)          = Left (Left a)
leftEitherAssoc (Right (Left b))  = Left (Right b)
leftEitherAssoc (Right (Right c)) = Right c

rightEitherAssoc :: Either (Either a b) c -> Either a (Either b c)
rightEitherAssoc (Left (Left a))  = (Left a)
rightEitherAssoc (Left (Right b)) = (Right (Left b))
rightEitherAssoc (Right c)        = (Right (Right c))
