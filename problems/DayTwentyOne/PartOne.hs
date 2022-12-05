module DayTwentyOne.PartOne where

import Lib.Solution
import Helpers.Solution

type Out = Int

examples :: [(String, Out)]
examples = []

solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Either String Out) (nyi "Solution")
