module Task6
  ( distributivity
  ) where

import Data.Maybe (mapMaybe)

-- Определить слабую головную нормальную форму:
distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

null $ mapMaybe foo "pole chudes ochen' chudesno"

-- где
foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

-- not actually, but good enough for this task
-- null :: [a] -> Bool
null [] = True
null _ = False

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  let rs = mapMaybe f xs
   in case f x of
        Nothing -> rs
        Just r -> r : rs
