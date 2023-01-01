{-|
module: DaySeventeen.PartOne
description: Avent of Code 2022 Day Seventeen, Part One
-}
module DaySeventeen.PartOne(
    Out,
    solution,
    charToMovement,
    Movement(..),
    shakaWhenTheRocksFell,
    rocks,
    Point(..)
) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import GHC.Data.Maybe(orElse)
import Data.List(intersperse)
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

-- | A way a rock can move... blown left, blown right, or falling down.
data Movement = DriftL | DriftR | Fall deriving (Eq, Show)

-- | Parse a movement from a character
charToMovement :: Char -> Result Movement
charToMovement '>' = Right DriftR
charToMovement '<' = Right DriftL
charToMovement 'v' = Right Fall
charToMovement x = Left $ "Unrecognized direction: " ++ show x

-- | A Point in 2d space
data Point = Point
    { xCoord :: Int
    , yCoord :: Int
    } deriving (Eq, Ord, Show)

add :: Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | All the base rocks, with minX == 0, minY == 0
rocks :: [Set.Set Point]
rocks = cycle $ map (Set.fromList . map (uncurry Point)) [
      [(0,0), (1, 0), (2, 0), (3, 0)] -- Horizontal Line
    , [(1, 0), (1, 1), (1, 2), (0, 1), (2, 1)] -- Plus
    , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)] -- L, backwards
    , [(0, 0), (0, 1), (0, 2), (0, 3)] -- Vertical Line
    , [(0, 0), (1, 0), (0, 1), (1, 1)] -- Square
    ]

extremes :: Set.Set Point -> (Int, Int, Int)
extremes s = (minX, maxX, minY)
    where minX = Set.lookupMin xs `orElse` 0
          maxX = Set.lookupMax xs `orElse` 0
          minY = Set.lookupMin ys `orElse` 0
          xs = Set.map xCoord s
          ys = Set.map yCoord s

translate :: Movement -> Set.Set Point -> Set.Set Point
translate dir = Set.map (add vec)
    where vec = case dir of
                    DriftR -> Point 1 0
                    DriftL -> Point (-1) 0
                    Fall -> Point 0 (-1)

drift :: Set.Set Point -> Set.Set Point -> [Movement] -> (Set.Set Point, [Movement])
drift _ _ [] = error "drift called with empty list"
drift settled rock (m:ms)
    | not collided = drift settled rock' ms
    | otherwise = if m == Fall then (rock, ms) else drift settled rock ms
    where collided = minX < 0 || minY < 0 || maxX > 6 || not (Set.disjoint rock' settled)
          (minX, maxX, minY) = extremes rock'
          rock' = translate m rock

simulate :: Set.Set Point -> [Movement] -> Set.Set Point -> (Set.Set Point, [Movement])
simulate settled ms rockArchetype = (topRows, remaining)
    where topRows = Set.filter ((>maxY - 100) . yCoord) $ Set.union settled final
          (final, remaining) = drift settled spawned ms
          spawned = Set.map (add (Point 2 (maxY + 4))) rockArchetype
          maxY = Set.lookupMax (Set.map yCoord settled) `orElse` -1

{- |
From a list of movements, calculate the infinite list of states - where the first item
in the tuple is the top 100 rows in the stack, and the second item is the winds
that are "left" - an infininte list.
-}
shakaWhenTheRocksFell :: [Movement] -> [(Set.Set Point, [Movement])]
shakaWhenTheRocksFell jetPattern = scanl (uncurry simulate) (Set.empty, movements) rocks
    where movements = intersperse Fall $ cycle jetPattern

fallRocks :: [Movement] -> Int
fallRocks jetPattern = 1 + (Set.lookupMax (Set.map yCoord final) `orElse` 0)
    where final = states !! 2022
          states =  map fst . shakaWhenTheRocksFell $ jetPattern

examples :: [(String, Out)]
examples = [("1", 3068)]

-- | Solution for Day Seventeen, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (charByCharM charToMovement . stripNewLines) (Right . fallRocks)
