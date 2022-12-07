{-|
module: DaySix.PartTwo
description: Avent of Code 2022 Day Six, Part Two
-}
module DaySix.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import Distribution.Simple.Utils(safeTail)
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 19), ("2", 23), ("3", 23), ("4", 29), ("5",26)]

findStartMarker :: Int -> String -> Result Int
findStartMarker _ "" = Left "Reached end of String"
findStartMarker n s
    | Set.size marker == n = Right n
    | otherwise = (+1) <$> findStartMarker n s'
    where marker = Set.fromList $ take n s
          s' = safeTail s

-- | Solution for Day Six, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (Right . stripNewLines) (findStartMarker 14)
