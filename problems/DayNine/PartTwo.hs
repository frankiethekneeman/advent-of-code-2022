{-|
module: DayNine.PartTwo
description: Avent of Code 2022 Day Nine, Part Two
-}
module DayNine.PartTwo(Out, solution) where

import Lib.Solution
import qualified Data.Set as Set
import DayNine.PartOne(parseInstructions, Dir, go, follow)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 1), ("2", 36)]

move :: [(Int, Int)] -> Dir -> [(Int, Int)]
move [] _ = []
move (h:t) d = scanl follow h' t
    where h' = go h d

modelString :: Int -> [Dir] -> Int
modelString n = Set.size . Set.fromList . map last . scanl move (replicate n (0,0))

-- | Solution for Day Nine, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parseInstructions (Right . modelString 10)
