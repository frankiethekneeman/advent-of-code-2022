{-|
module: DayEight.PartTwo
description: Avent of Code 2022 Day Eight, Part Two
-}
module DayEight.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day Eight, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Either String Out) (nyi "Solution")
