{-|
module: DaySeventeen.PartTwo
description: Avent of Code 2022 Day Seventeen, Part Two
-}
module DaySeventeen.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Input
import Data.List(zip4, genericLength, genericTake)
import GHC.Data.Maybe(fromMaybe)
import DaySeventeen.PartOne(charToMovement, Movement(..), shakaWhenTheRocksFell, Point(..))
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [("1", 1514285714288)]

maxY :: Set.Set Point -> Int
maxY = fromMaybe (-1) . Set.lookupMax . Set.map yCoord

minX :: Set.Set Point -> Int
minX = fromMaybe (-1) . Set.lookupMin . Set.map xCoord

-- Get all points in the second argument that aren't in the first.
newPoints :: Set.Set Point -> Set.Set Point -> Set.Set Point
newPoints = flip Set.difference

window2 :: [a] -> [(a, a)]
window2 = zip <*> tail

rockNames :: [String]
rockNames = cycle ["H-Line", "Plus", "Corner", "V-Line", "Square"]

firstRepeat :: Ord a => [a] -> (Int, Int)
firstRepeat xs = head . dropWhile (uncurry (==)) . zip [0..] . zipWith (Map.!) rearVision $ xs
    where rearVision = tail . scanl mapAcc Map.empty $ withIdx
          mapAcc = flip . uncurry $ Map.insertWith min
          withIdx = zip xs [0..]

-- How many times do you have to cycle the first list to match the second at least n places
calcOffset :: Eq a => Int -> [a] -> [a] -> Int
calcOffset n start goal = fst . head . dropWhile ((/=needle) . snd) . zip [0..] $ haystack
    where needle = take n goal
          haystack = map (take n) . iterate tail . cycle $ start

mapPairs :: (a -> a -> b) -> [a] -> [b]
mapPairs f = map (uncurry f) . window2

getHeightIncreaseCycle :: [Movement] -> ([Integer], [Integer])
getHeightIncreaseCycle winds = (preamble, cyclic)
    where cyclic = take (lengthToRepeat - preambleLength) repeating
          (preamble, repeating) = splitAt preambleLength heightAdded
          (lengthToRepeat, preambleLength) = firstRepeat states
          states = zip4 rockNames xDisplacement heightAdded windOffset
          windOffset = map (`mod` windsLength)
                        . tail -- Drop leading zero
                        . scanl (+) 0 -- running Sum
                        . map (`div` 2) -- Account for interspersed downs
                        . mapPairs (calcOffset (2 * windsLength)) -- Get relative rotations
                        $ movements
          heightAdded = map toInteger
                         . mapPairs (flip (-))
                         . map maxY
                         $ points
          xDisplacement = map minX newRock
          newRock = mapPairs newPoints points
          windsLength = length winds
          (points, movements) = unzip . shakaWhenTheRocksFell $ winds

iterations :: Integer
iterations = 1000000000000
solve :: [Movement] -> Out
solve ms = preambleHeight + (cycleLoops * repeatingHeight) + (sum . genericTake remainingRocks $ repeating)
    where (cycleLoops, remainingRocks) = (iterations - preambleLength) `divMod` repeatingLength
          repeatingLength = genericLength repeating
          repeatingHeight = sum repeating
          preambleLength = genericLength preamble
          preambleHeight = sum preamble
          (preamble, repeating) = getHeightIncreaseCycle ms









-- | Solution for Day Seventeen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (charByCharM charToMovement . stripNewLines) (Right . solve)
