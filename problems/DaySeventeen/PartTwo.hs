{-|
module: DaySeventeen.PartTwo
description: Avent of Code 2022 Day Seventeen, Part Two
-}
module DaySeventeen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day Seventeen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
