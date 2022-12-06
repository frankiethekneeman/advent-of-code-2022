{-|
module: DayTwentyOne.PartOne
description: Avent of Code 2022 Day TwentyOne, Part One
-}
module DayTwentyOne.PartOne(Out, solution) where

import Lib.Solution
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day TwentyOne, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Either String Out) (nyi "Solution")
