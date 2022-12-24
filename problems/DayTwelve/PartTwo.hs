{-|
module: DayTwelve.PartTwo
description: Avent of Code 2022 Day Twelve, Part Two
-}
module DayTwelve.PartTwo(Out, solution) where

import Lib.Solution
--import Lib.Types
import Data.Either(rights)
import DayTwelve.PartOne(parseMap, countSteps, Map(..), ElevationMap, mapWithIndex)

-- | The type of the answer to this problem
type Out = Int

findStartingPoints :: ElevationMap -> [(Int, Int)]
findStartingPoints = map fst . filter ((=='a') . snd) . concat . addIndices
    where addIndices = mapWithIndex (\i l -> mapWithIndex (tuplize i) l) :: [[a]] -> [[((Int, Int), a)]]
          tuplize x y a = ((x, y), a)

findScenicRoute :: Map -> Int
findScenicRoute (Map _ end m) = minimum . rights . map (countSteps . initialize) $ startingPoints
    where initialize s = Map s end m
          startingPoints = findStartingPoints m

examples :: [(String, Out)]
examples = [("1", 29)]

-- | Solution for Day Twelve, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parseMap (Right . findScenicRoute)
