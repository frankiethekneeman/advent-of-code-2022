{-|
module: DayFifteen.PartOne
description: Avent of Code 2022 Day Fifteen, Part One
-}
module DayFifteen.PartOne(
    Out,
    solution,
    Range,
    Sensor(..),
    parseSensor,
    eliminate,
    visibleInRow
) where

import Lib.Solution
import Lib.Types
import Helpers.Parsing
import Helpers.Solution
import Helpers.Input
import Data.List(nub)
import Data.Maybe(catMaybes)

-- | The type of the answer to this problem
type Out = Integer
type Range = (Integer, Integer)

data Sensor = Sensor Integer Integer Integer Integer

instance Grokkable Sensor where
    fromResult = grok4 Sensor

parseSensor :: String -> Result Sensor
parseSensor = parse $ "Sensor at " ^& point ^& ": closest beacon is at " ^& point ^& ()
    where point = "x=" ^& scanInt ^& ", y=" ^& scanInt

rangeInRow :: Integer -> Sensor -> Maybe (Integer, Integer)
rangeInRow row (Sensor sx sy bx by) = if left <= right then Just (left, right) else Nothing 
    where left = sx - dist
          right = sx + dist
          dist = range - ydiff
          range = abs (sx - bx) + abs (sy - by)
          ydiff = abs (sy - row)

ifm :: Bool -> a -> Maybe a
ifm True = Just
ifm False = const Nothing

-- | Eliminate the RH range from the LH range  May return zero, 1 or 2 ranges
eliminate :: Range -> Range -> [Range]
eliminate (lmin, lmax) (rmin, rmax) = catMaybes [leftOverage, rightOverage]
    where leftOverage = ifm (lmin < rmin) (lmin, min lmax (rmin - 1))
          rightOverage = ifm (lmax > rmax) (max lmin (rmax + 1), lmax)

add :: Range -> [Range] -> [Range]
add new existing = new:existing'
    where existing' = flip eliminate new =<< existing

rangeSize :: Range -> Integer
rangeSize (l, r) = r - l + 1

getBeacon :: Sensor -> (Integer, Integer)
getBeacon (Sensor _ _ bx by) = (bx, by)

visibleInRow :: Integer -> [Sensor] -> [Range]
visibleInRow row = foldl (flip add) [] . catMaybes . map (rangeInRow row) 

countObscured :: Integer -> [Sensor] -> Out
countObscured row sensors = inRange - beacons
    where beacons = toInteger . length . nub . filter ((==row).snd) . map getBeacon $ sensors
          inRange = sum . map rangeSize $ eliminated
          eliminated = visibleInRow row sensors

examples :: [(String, Integer, Out)]
examples = [("1", 10, 26)]

-- | Solution for Day Fifteen, Part One
solution:: AdventProblem Out
solution = adventOfCodeConfigurable examples 2000000 (lineByLineM parseSensor) (configurableRight countObscured)
