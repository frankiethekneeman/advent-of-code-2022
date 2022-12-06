{-|
module: DayFive.PartTwo
description: Avent of Code 2022 Day Five, Part Two
-}
module DayFive.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day Five, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Either String Out) (nyi "Solution")
