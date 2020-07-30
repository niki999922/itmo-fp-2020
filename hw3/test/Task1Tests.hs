module Task1Tests(geometry) where

import Criterion.Main
import Task1
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- generate n + 1 point
pointGenerator :: Int -> [Point]
pointGenerator n = (Point 0 0) : pointGenerator' (n - 1) (n - 1, 0)
  where
    pointGenerator' :: Int -> (Int, Int) -> [Point]
    pointGenerator' n' (x', y')
      | n' < 0 = []
      | otherwise = Point x' y' : pointGenerator' (n' - 1) (x' - 1, y' + 1)

perimeterLazy :: [Point] -> Double
perimeterLazy points = go 0 points + dist (last points) (head points)
  where
    dist :: Point -> Point -> Double
    dist p1 p2 = sqrt . fromIntegral $ (x p2 - x p1) ^ (2 :: Int) + (y p2 - y p1) ^ (2 :: Int)
    go :: Double -> [Point] -> Double
    go acc (p1:p2:ps) = go (acc + dist p1 p2) (p2:ps)
    go acc _          = acc

doubleAreaLazy :: [Point] -> Int
doubleAreaLazy points = sum' (\p1 p2 -> crossProduct p1 p2) 0 points + xny1 - x1yn
  where
    last' = last points
    first' = head points
    xny1 = x last' * y first'
    x1yn = x first' * y last'
    sum' :: (Point -> Point -> Int) -> Int -> [Point] -> Int
    sum' f acc (p1:p2:ps) = sum' f (acc + f p1 p2) (p2:ps)
    sum' _ acc _          = acc

geometry :: SpecWith ()
geometry =
  describe "Testing geometry" $ do
    let p1 = Point 0 0
    let p2 = Point 2 0
    let p3 = Point 2 2
    let p4 = Point 0 2
    let points = pointGenerator $ 10^(7 :: Int)
    it "Test plus" $ do
      (p1 `plus` p2) `shouldBe` (Point 2 0)
    it "Test minus" $ do
      (p1 `minus` p2) `shouldBe` (Point (-2) (0))
    it "Test scalarProduct" $ do
      (p3 `scalarProduct` p4) `shouldBe` 4
    it "Test crossProduct" $ do
      (p3 `crossProduct` p2) `shouldBe` (-4)
    it "Test perimeter" $ do
      (perimeter [p1, p2, p3, p4]) `shouldBe` (8)
    it "Test doubleArea" $ do
      (doubleArea [p1, p2, p3, p4]) `shouldBe` (8)
    it "Test performance perimeter" $ do
      defaultMain [
        bgroup "perimeter" [ bench "lazy 10^7"  $ whnf perimeterLazy points,
                             bench "fast 10^7"  $ whnf perimeter points
                           ]
        ]
    it "Test performance area" $ do
      defaultMain [
        bgroup "area" [ bench "lazy 10^7"  $ whnf doubleAreaLazy points,
                        bench "fast 10^7"  $ whnf doubleArea points
                      ]
        ]
