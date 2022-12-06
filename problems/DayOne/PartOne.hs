{-|
module: DayOne.PartOne 
description: Advent of Code 2022 Day One, Part One 
-}
module DayOne.PartOne(solution) where

import Lib.Solution
import Data.List.Split

tests :: [(String, Int)]
tests = [("1", 24000)]

parser :: String -> [[Int]]
parser input = map (map read) inventories
    where inventories = map (lines) elves
          elves = splitOn "\n\n" input

solver :: [[Int]] -> Int
solver = maximum . map sum

-- | Solution for Day One, Part One
solution :: AdventProblem Int
solution = adventOfCode tests (Right . parser) (Right . solver)
