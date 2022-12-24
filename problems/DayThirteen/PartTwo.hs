{-|
module: DayThirteen.PartTwo
description: Avent of Code 2022 Day Thirteen, Part Two
-}
module DayThirteen.PartTwo(Out, solution) where

import Lib.Solution
import Data.List(sort)
import DayThirteen.PartOne(Packet(..), parseInput)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 140)]

dividerPackets :: [Packet]
dividerPackets = [List [List [Number 2]], List [List [Number 6]]]

getDecoderKey :: [(Packet, Packet)] -> Int
getDecoderKey = product . map fst .
        filter (isDivider . snd) . zip [1..]  . 
        sort . (dividerPackets ++) . (detuple =<<)
    where detuple (l, r) = [l, r]
          isDivider = flip elem dividerPackets

-- | Solution for Day Thirteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (Right . getDecoderKey)
