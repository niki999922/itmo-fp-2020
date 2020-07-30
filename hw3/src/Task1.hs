{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Task1 where

data Point =
  Point
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

plus :: Point -> Point -> Point
plus p1 p2 = Point (x p1 + x p2) (y p1 + y p2)

minus :: Point -> Point -> Point
minus p1 p2 = Point (x p1 - x p2) (y p1 - y p2)

scalarProduct :: Point -> Point -> Int
scalarProduct p1 p2 = x p1 * x p2 + y p1 * y p2

crossProduct :: Point -> Point -> Int
crossProduct p1 p2 = x p1 * y p2 - x p2 * y p1

-- counts the perimeter
perimeter :: [Point] -> Double
perimeter [] = 0
perimeter points@(first':_) = sum' (dist first') 0 points
  where
    dist :: Point -> Point -> Double
    dist (Point x1 y1) (Point x2 y2) =
      sqrt . fromIntegral $ (x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)
    sum' :: (Point -> Double) -> Double -> [Point] -> Double
    sum' f !acc (p1:ost@(p2:_)) = sum' f (acc + dist p1 p2) ost
    sum' f !acc [ps]            = acc + f ps
    sum' _ !acc _               = acc

-- counts the double area
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea points@(first':_) =
  sum' (\p2 -> x first' * y p2 - x p2 * y first') 0 points
  where
    sum' :: (Point -> Int) -> Int -> [Point] -> Int
    sum' f !acc (p1:ost@(p2:_)) = sum' f (acc + crossProduct p1 p2) ost
    sum' f !acc [ps]            = acc + f ps
    sum' _ !acc _               = acc
