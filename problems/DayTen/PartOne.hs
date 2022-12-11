{-|
module: DayTen.PartOne
description: Avent of Code 2022 Day Ten, Part One
-}
module DayTen.PartOne(Out, solution, Cmd, parseCmd, runProg, lookupFloor) where

import Lib.Solution
import Lib.Types
import Helpers.Parsing
import Helpers.Input
import Control.Applicative.Tools((<.>))
import Data.Either.Combinators(maybeToRight)
import qualified Data.Map as Map

-- | The type of the answer to this problem
type Out = Integer

data Cmd = Noop | Addx Integer

instance Grokkable Cmd where
    fromResult r = case get 0 r of
        Right "noop" -> Right Noop
        Right "addx" -> Addx <$> get 1 r
        Right i ->  Left $ "Unrecognized instruction " ++ i
        Left s -> Left s

parseCmd :: String -> Result Cmd
parseCmd = parse (cmds ^& " " ^& scanInt)
    where cmds = alternating $ map remember ["noop", "addx"]

examples :: [(String, [Integer], Out)]
examples = [("1", [1..6], 36), ("2", [20,60..220], 13140)]

execute :: (Integer, Integer) -> Cmd -> (Integer, Integer)
execute (c, r) Noop = (c + 1, r)
execute (c, r) (Addx n) = (c + 2, r + n)

runProg :: [Cmd] -> Map.Map Integer Integer
runProg cmds = Map.fromDistinctAscList changes
    where changes = map snd $ filter registerChanged windows
          registerChanged ((_, l), (_, r)) = r /= l
          windows = zip ((0,0):history) history
          history = scanl execute (1,1) cmds

lookupFloor :: Show k => Ord k => Enum k => k -> Map.Map k a -> Result a
lookupFloor key = maybeToRight errMsg . snd <.> Map.lookupMax . fst . Map.split next
    where errMsg = "Key too small: " ++ show key
          next = succ key

analyzeSignal :: [Integer] -> [Cmd] -> Result Integer
analyzeSignal points cmd = sum <$> measurements
    where measurements = mapM measure points :: Result [Integer]
          measure p = (p*) <$> lookupFloor p signal
          signal = runProg cmd
-- | Solution for Day Ten, Part One
solution:: AdventProblem Out
solution = adventOfCodeConfigurable
    examples
    [20,60..220]
    (lineByLineM parseCmd)
    analyzeSignal
