{-|
module: DaySeven.PartTwo
description: Avent of Code 2022 Day Seven, Part Two
-}
module DaySeven.PartTwo(Out, solution) where

import Lib.Solution
import DaySeven.PartOne(Out, getSize, ResolvedFile(..), tree, parseSession)

examples :: [(String, Out)]
examples = [("1", 24933642)]

minFolderToFree :: Integer -> Integer -> ResolvedFile -> Integer
minFolderToFree size target f = minimum . filter (>= needed) . map getSize . filter isDir . tree $ f
    where needed =  getSize f - (size - target)
          isDir (ResolvedDir _ _) = True
          isDir _ = False

-- | Solution for Day Seven, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parseSession (Right . minFolderToFree 70000000 30000000)
