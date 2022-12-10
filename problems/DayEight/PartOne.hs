{-|
module: DayEight.PartOne
description: Avent of Code 2022 Day Eight, Part One
-}
module DayEight.PartOne(Out, solution, parseHeights) where

import Lib.Solution
import Lib.Types
import Text.Read(readMaybe)
import Data.List(transpose)
import Helpers.Input(lineByLineM)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 21)]

parseHeights :: String -> Result [Int]
parseHeights = sequence . map (toHeight . readMaybe . pure)
    where toHeight Nothing = Left "Not a digit"
          toHeight (Just x) = Right x

getVisibilityMap :: [[Int]] -> [[Int]]
getVisibilityMap trees = foldl mins base $ fmap ($ trees) maps
    where mins = zipWith $ zipWith min
          base = repeat $ repeat 10
          maps = [leftMap, rightMap, topMap, bottomMap]
          leftMap = sightLines
          rightMap = map reverse . sightLines . map reverse
          topMap = transpose . sightLines . transpose
          bottomMap = transpose . map reverse . sightLines . map reverse . transpose
          sightLines = map (init . scanl max (-1))

countVisible :: [[Int]] -> Int
countVisible trees = sum . map (length . filter id) $ visibleTrees
    where visibleTrees = zipWith (zipWith (>)) trees visMap
          visMap = getVisibilityMap trees
    

-- | Solution for Day Eight, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseHeights) (Right . countVisible)
