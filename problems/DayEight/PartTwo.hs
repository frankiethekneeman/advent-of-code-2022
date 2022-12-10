{-|
module: DayEight.PartTwo
description: Avent of Code 2022 Day Eight, Part Two
-}
module DayEight.PartTwo(Out, solution) where

import Lib.Solution
import DayEight.PartOne(parseHeights)
import Data.List(transpose)
import Helpers.Input(lineByLineM)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 8)]

-- | Candidate for promotion to library?  Takes a directional "look" on a
-- 2d map and applies it in all four directions, then combines them to
-- create a point map.
getFourWayMap :: ([a] -> [b]) -- | Some kind of directional calculation
    -> (b -> b -> b) -- | A way to combine the results
    -> b -- | A based to build the combinations from
    -> [[a]] -> [[b]]
getFourWayMap f combinator zero input = foldl combine base $ fmap ($ input) maps
    where combine = zipWith $ zipWith combinator
          base = repeat $ repeat zero
          maps = [leftMap, rightMap, topMap, bottomMap]
          leftMap = way
          rightMap = map reverse . way . map reverse
          topMap = transpose . way . transpose
          bottomMap = transpose . map reverse . way . map reverse . transpose
          way = map f

getOutlookMap :: [[Int]] -> [[Int]]
getOutlookMap = getFourWayMap outlook (*) 1
    where outlook (x:xs) = length (takeUntil (>=x) xs) : outlook xs
          outlook [] = []

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) 
    | p x = [x]
    | otherwise = x:takeUntil p xs

bestOutlook :: [[Int]] -> Int
bestOutlook = maxMax . getOutlookMap
    where maxMax = (fmap <*> fmap) maximum

-- | Solution for Day Eight, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseHeights) (Right . bestOutlook)
