{-|
module: DayTen.PartTwo
description: Avent of Code 2022 Day Ten, Part Two
-}
module DayTen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import qualified Data.Map as Map
import Data.List.Split(chunksOf)
import Data.List(intercalate)
import Helpers.Input
import DayTen.PartOne(Cmd, lookupFloor, parseCmd, runProg)

-- | The type of the answer to this problem
type Out = String

examples :: [(String, Out)]
examples = [
    ("1", "#####...................................\n#.......................................\n#.......................................\n#.......................................\n#.......................................\n#......................................."),
    ("2", "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....")
    ]

isOn :: Map.Map Integer Integer -> Integer -> Result Bool
isOn signal clock = check pixel <$> reg
    where check px loc = abs (loc - px) <= 1
          pixel = (clock - 1) `mod`40
          reg = lookupFloor clock signal

wrap :: Int -> String -> String
wrap n = intercalate "\n" . chunksOf n

asPixel :: Bool -> Char 
asPixel True = '#'
asPixel _ = '.'

calculateDisplay :: [Cmd] -> Result String
calculateDisplay cmd = wrap 40 . map asPixel <$> measurements
    where measurements = mapM (isOn signal) [1..240] 
          signal = runProg cmd

-- | Solution for Day Ten, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseCmd) calculateDisplay
