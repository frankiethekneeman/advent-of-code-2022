module DayOne.PartTwo where

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

solution :: AdventProblem Int
solution = adventOfCode tests (Right . parser) (Right . solver)
