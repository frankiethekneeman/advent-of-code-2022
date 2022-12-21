{-|
module: DayEleven.PartTwo
description: Avent of Code 2022 Day Eleven, Part Two
-}
module DayEleven.PartTwo(Out, solution) where

import Lib.Solution
--import Lib.Types
import DayEleven.PartOne(parseMonkeys, Monkey(..))
import Data.List(genericLength, sort)
--import qualified Data.Set as Set
--import Control.Monad(join)
import GHC.Utils.Misc(nTimes)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [("1", 2713310158)]

inspectAll :: Integer -> Monkey -> (Monkey, [(Integer, Integer)])
inspectAll modulus m = (m {itemsHeld = [], inspections = inspections'}, items)
    where inspections'= inspections m + genericLength (itemsHeld m)
          items = map inspect $ itemsHeld m
          inspect item = (target, item')
            where target = test m item'
                  item' = operation m item `mod` modulus

give :: Integer -> Monkey -> Monkey
give item m = m{itemsHeld = itemsHeld'}
    where itemsHeld' = item:itemsHeld m

turn :: Integer -> [Monkey] -> [Monkey]
turn i monkeys = foldl put monkeys' (concat thrown)
    where (monkeys', thrown) = unzip $ map maybeInspectAll monkeys :: ([Monkey], [[(Integer, Integer)]])
          maybeInspectAll m = if i == identity m then inspectAll modulus m else (m, [])
          modulus = product (map divisor monkeys)
          put ms (target, item) = map catch ms
            where catch m' = if target == identity m' then give item m' else m'

doRound :: [Monkey] -> [Monkey]
doRound m = foldl (flip turn) m (map identity m)

solve :: [Monkey] -> Integer
solve = product . take 2 . reverse . sort . map inspections . nTimes 10000 doRound

-- | Solution for Day Eleven, Part Two
solution :: AdventProblem Out
solution = adventOfCode examples parseMonkeys (Right . solve)
