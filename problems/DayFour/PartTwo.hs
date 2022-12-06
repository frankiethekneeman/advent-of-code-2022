{-|
module: DayFour.PartTwo 
description: Advent of Code 2022 Day Four, Part Two 
-}
module DayFour.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Input
import Helpers.Parsing

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 4)]

data Range = Range Integer Integer deriving Eq

instance Grokkable (Range, Range) where 
    fromResult = grok4 toRanges
        where toRanges lmin lmax rmin rmax = (Range lmin lmax, Range rmin rmax)

scanRangePair :: Scanner
scanRangePair = scanRange ^& "," ^& scanRange
    where scanRange = scanInt ^& "-" ^& scanInt

parseRangePair :: String -> Either String (Range, Range)
parseRangePair = parse scanRangePair

superRange :: Range -> Range -> Range
superRange (Range lmin lmax) (Range rmin rmax) = Range (min lmin rmin) (max lmax rmax)

size :: Range -> Integer
size (Range l r) = r - l + 1

haveOverlap :: Range -> Range -> Bool
haveOverlap l r = size l + size r > superSize
    where superSize = size $ superRange l r

countOverlaps :: [(Range, Range)] -> Int
countOverlaps = length . filter (uncurry haveOverlap)

-- | Solution for Day Four, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseRangePair) (Right . countOverlaps)
