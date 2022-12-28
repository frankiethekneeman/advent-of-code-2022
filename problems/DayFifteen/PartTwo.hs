{-|
module: DayFifteen.PartTwo
description: Avent of Code 2022 Day Fifteen, Part Two
-}
module DayFifteen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import DayFifteen.PartOne(parseSensor, Range, Sensor, eliminate, visibleInRow)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Integer, Out)]
examples = [("1", 20, 56000011)]

notEliminated :: Range -> [Range] -> [Range]
notEliminated base = foldl eliminations [base]
    where eliminations acc r = flip eliminate r =<< acc

getTuningFrequency :: Integer -> [Sensor] -> Result Out
getTuningFrequency maxR sensors = case found of
                                    [(y, [(x1,x2)])] | x1 == x2 -> Right $ 4000000 * x1 + y
                                    [] -> Left "No Location Found"
                                    _ -> Left "Too many possibilities found"
    where found = filter ((/=[]).snd) possibilities
          possibilities = zip <*> map toPossibilities $ [0..maxR]
          toPossibilities r = notEliminated (0,maxR) . visibleInRow r $ sensors
-- | Solution for Day Fifteen, Part Two
solution:: AdventProblem Out
solution = adventOfCodeConfigurable examples 4000000 (lineByLineM parseSensor) getTuningFrequency
