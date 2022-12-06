{-|
module: DayTwentyTwo.PartOne
description: Avent of Code 2022 Day TwentyTwo, Part One
-}
module DayTwentyTwo.PartOne(Out, solution) where

import Lib.Solution
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day TwentyTwo, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Either String Out) (nyi "Solution")
