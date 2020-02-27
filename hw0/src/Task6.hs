module Task6
  ( x
  , y
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing

-- WHNF "x = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))", because until call field string wouldn't concat, but no reductions.
x :: (Either String a, Either String b)
x = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- WHNF "y = False"
y :: Bool
y = null $ mapMaybe foo "pole chudes ochen' chudesno"
