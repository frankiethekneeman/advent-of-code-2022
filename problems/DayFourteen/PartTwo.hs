{-|
module: DayFourteen.PartTwo
description: Avent of Code 2022 Day Fourteen, Part Two
-}
module DayFourteen.PartTwo(Out, solution) where

import Lib.Solution
import qualified Data.Set as Set
import DayFourteen.PartOne(Point(..), getY, parseRocks, toRocks)
import Data.List(genericTake)
import Helpers.Input

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 93)]

descendents :: Point -> [Point]
descendents (Point x y) = map (flip Point (y + 1)) [x - 1 .. x + 1]

cascade :: Set.Set Point -> Set.Set Point -> Set.Set Point
cascade forbidden row = Set.difference descendents' forbidden
    where descendents' = Set.unions . map (Set.fromList . descendents) . Set.toList $ row

findAllReachablePoints :: [[Point]] -> Int
findAllReachablePoints points = sum . genericTake (maxDepth + 2) . map Set.size $ sands
    where maxDepth = maximum . map getY . Set.toList $ rocks
          sands = iterate (cascade rocks) (Set.singleton (Point 500 0))
          rocks = toRocks points

-- | Solution for Day Fourteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseRocks) (Right . findAllReachablePoints)
