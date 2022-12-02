module DayOne.PartOne where

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

solution :: AdventProblem Int
solution = adventOfCode tests (Right . parser) (Right . solver)
