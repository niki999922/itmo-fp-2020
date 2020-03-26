module Block5
  ( eval
  , Expression(..)
  ) where

data Expression
  = Const Int
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Power Expression Expression
  | Negation Expression
  deriving (Show)

eval :: Expression -> Either String Int
eval (Const x) = Right x
eval (Addition left right) = (+) <$> (eval left) <*> (eval right)
eval (Subtraction left right) = (-) <$> (eval left) <*> (eval right)
eval (Multiplication left right) = (*) <$> (eval left) <*> (eval right)
eval (Division left right) = do
  rightEval <- eval right
  if (rightEval == 0)
    then Left $ "Divizion by zero" ++ show right
    else (div) <$> (eval left) <*> pure rightEval
eval (Power left right) = do
  rightEval <- eval right
  if (rightEval < 0)
    then Left $ "Power on negate power =)" ++ show right
    else (^) <$> (eval left) <*> pure rightEval
eval (Negation x) = negate <$> (eval x)
