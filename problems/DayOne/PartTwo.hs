{-|
module: DayOne.PartTwo 
description: Advent of Code 2022 Day One, Part Two 
-}
module DayOne.PartTwo(solution) where

import Lib.Solution
import Data.List.Split
import Data.List

tests :: [(String, Int)]
tests = [("1", 45000)]

parser :: String -> [[Int]]
parser input = map (map read) inventories
    where inventories = map (lines) elves
          elves = splitOn "\n\n" input

solver :: [[Int]] -> Int
solver = sum . take 3 . reverse . sort . map sum

-- | Solution for Day One, Part Two
solution :: AdventProblem Int
solution = adventOfCode tests (Right . parser) (Right . solver)
