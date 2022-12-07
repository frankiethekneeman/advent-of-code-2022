{-|
module: DaySix.PartOne
description: Avent of Code 2022 Day Six, Part One
-}
module DaySix.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import Distribution.Simple.Utils(safeTail)
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 7), ("2", 5), ("3", 6), ("4", 10), ("5",11)]

findStartMarker :: Int -> String -> Result Int
findStartMarker _ "" = Left "Reached end of String"
findStartMarker n s
    | Set.size marker == n = Right n
    | otherwise = (+1) <$> findStartMarker n s'
    where marker = Set.fromList $ take n s
          s' = safeTail s

-- | Solution for Day Six, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (Right . stripNewLines) (findStartMarker 4)
