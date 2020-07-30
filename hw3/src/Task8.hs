{-# LANGUAGE InstanceSigs #-}

module Task8 where

import Control.Comonad
import Control.Monad (liftM2)
import Data.List
import System.Random
import Task8Extension

newtype Grid a =
  Grid
    { unGrid :: ListZipper (ListZipper a)
    }

instance Functor Grid where
  fmap f (Grid (LZ ls x rs)) =
    Grid $ LZ (map (fmap f) ls) (fmap f x) (map (fmap f) rs)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead
  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

up, down :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical = genericMove up down

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [left, right]
    verticals = [up, down]

{-
  Healthy - здоров " "
  Immunized - имунитет "@"
  Sick - заражён "#"
  Ill - болеет "+"
-}
data Condition
  = Healthy StdGen
  | Immunized StdGen Int
  | Sick StdGen Int
  | Ill StdGen Int

-- Day constants
daysImmunized, daysSick, daysIll :: Int
daysImmunized = 2
daysSick = 3
daysIll = 4

-- Field size to display
fieldSize :: Int
fieldSize = 20

-- Chance to get infected
sickChance :: Double
sickChance = 0.36

showField :: Grid Condition -> Int -> String
showField field limit = do
  let (Grid newField) = (fmap handler field)
  let withoutUpDown = toList newField limit
  let withoutLeftRight = map (`toList` limit) withoutUpDown
  intercalate "\n" (map (intercalate "") withoutLeftRight)
  where
    handler :: Condition -> String
    handler (Healthy _)     = " "
    handler (Immunized _ _) = "@"
    handler (Sick _ _)      = "#"
    handler (Ill _ _)       = "+"

generateStartField :: Grid Condition
generateStartField =
  let intField =
        Grid $ duplicate $ LZ (iterate (+ (-1)) (-1)) 0 (iterate (+ 1) 1)
   in let poleWithHumans = fmap changeLayout intField
       in gridWrite (Sick (mkStdGen 0) daysSick) poleWithHumans
  where
    changeLayout :: Int -> Condition
    changeLayout val = Healthy (mkStdGen val)

evolve :: Grid Condition -> Grid Condition
evolve = extend oneDay

oneDay :: Grid Condition -> Condition
oneDay g = handeCases (extract g) (allNeighbours g)
  where
    handeCases :: Condition -> [Condition] -> Condition
    handeCases (Ill gen ost) _ =
      if (ost == 0)
        then (Immunized gen daysImmunized)
        else (Ill gen (ost - 1))
    handeCases (Sick gen ost) _ =
      if (ost == 0)
        then (Ill gen daysIll)
        else (Sick gen (ost - 1))
    handeCases (Immunized gen ost) _ =
      if (ost == 0)
        then (Healthy gen)
        else (Immunized gen (ost - 1))
    handeCases (Healthy gen) neighbours' = do
      let neighboursChance = chanceBeZombie neighbours'
      let (personChance, newGen) = random gen :: (Double, StdGen)
      if (personChance <= neighboursChance)
        then Sick newGen daysSick
        else Healthy newGen
      where
        chanceBeZombie :: [Condition] -> Double
        chanceBeZombie list =
          1.0 -
          ((1.0 - sickChance) :: Double) ^
          ((foldl' (+) 0 (map haveChance list)) :: Int)
        haveChance :: Condition -> Int
        haveChance (Ill _ _)       = 1
        haveChance (Sick _ _)      = 1
        haveChance (Immunized _ _) = 0
        haveChance (Healthy _)     = 0

allNeighbours :: Grid Condition -> [Condition]
allNeighbours g = map (\direction -> extract $ direction g) neighbours

-- example for watch: "afterDays 10 generateStartField `showField` fieldSize"
afterDays :: Int -> Grid Condition -> Grid Condition
afterDays n g
  | n == 0 = g
  | otherwise = afterDays (n - 1) (evolve g)
