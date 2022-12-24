{-|
module: DayTwelve.PartOne
description: Avent of Code 2022 Day Twelve, Part One
-}
module DayTwelve.PartOne(Out, solution, parseMap, countSteps, Map(..), Position, ElevationMap, mapWithIndex) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Data.List(elemIndex)
import Distribution.Simple.Utils(safeHead)
import Data.Maybe(catMaybes, fromMaybe)
import qualified Algorithms.AStar as AStar

-- | The type of the answer to this problem
type Out = Int

-- | A position.  On a Map
type Position = (Int, Int)
-- | This makes my function types more meaningful
type ElevationMap = [[Char]]

-- | Start, End, elevations
data Map = Map Position Position ElevationMap deriving Show

getLocation :: Eq a => a -> [[a]] -> Maybe Position
getLocation needle haystack = safeHead $ catMaybes rows
    where rows = mapWithIndex toTuple searched
          toTuple i found = (i,) <$> found
          searched = map (elemIndex needle) haystack

-- | Standard map, but make the index available to the function as well.
mapWithIndex :: (Int -> a  -> b) -> [a] -> [b]
mapWithIndex f = map (uncurry f) . zipWithIndex

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

replace :: Eq a => a -> a -> [[a]] -> [[a]]
replace find repl = map $ map f
    where f x = if x == find then repl else x

-- | Find the Start and End on this 2-D map, and replace them with their elevations
parseMap :: String -> Result Map
parseMap input = Map <$> start <*> end <*> pure elevations'
    where start = maybeToResult "Starting location not found" $ getLocation 'S' elevations
          end = maybeToResult "Ending location not found" $ getLocation 'E' elevations
          elevations' = (replace 'E' 'z' . replace 'S' 'a') elevations
          elevations = lines input

getElevation :: ElevationMap -> Position -> Maybe Char
getElevation m (x, y)
    | x < 0 || x >= length m = Nothing
    | y < 0 || y >= length (m !! x) = Nothing
    | otherwise = Just $ m !! x !! y

steps :: ElevationMap -> Position -> [Position]
steps m (x, y) = filter reachable [(x, y+1), (x, y-1), (x + 1, y), (x-1, y)]
    where reachable = fromMaybe False . ((>=) <$> allowed <*>) . getElevation m
          allowed = succ <$> getElevation m (x, y)

-- | Count the number of steps from a starting position to an ending position given an elevation map
countSteps :: Map -> Result Int
countSteps (Map start (endX, endY) m) = maybeToResult "No Path Found" (pred . length <$> path)
    where path = AStar.uniformWeights distRemaining (steps m) start
          distRemaining (x, y) = abs (x - endX) + abs (y - endY)

examples :: [(String, Out)]
examples = [("1", 31)]

-- | Solution for Day Twelve, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseMap countSteps
