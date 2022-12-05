module DayFour.PartOne where

import Lib.Solution
import Helpers.Input
import Helpers.Parsing

type Out = Int

examples :: [(String, Out)]
examples = [("1", 2)]

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

haveCompleteOverlap :: Range -> Range -> Bool
haveCompleteOverlap l r = l == super || r == super
    where super = superRange l r

countCompleteOverlaps :: [(Range, Range)] -> Int
countCompleteOverlaps = length . filter (uncurry haveCompleteOverlap)


solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseRangePair) (Right . countCompleteOverlaps)
