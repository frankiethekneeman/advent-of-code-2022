{-|
module: DaySixteen.PartTwo
description: Avent of Code 2022 Day Sixteen, Part Two
-}
module DaySixteen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import DaySixteen.PartOne(Valve(..), parseValve, buildWorkingValveMap, closedFlows)
import GHC.Utils.Misc(liftFst)
import Data.Maybe(catMaybes)
import Data.Either.Combinators(maybeToRight)
import Data.List(sort)
import qualified Algorithms.AStar as AStar

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 1707), ("2", 312), ("3", 92), ("4", 88), ("5", 264), ("6", 2528)]

data Status = Opening Int String | Start | Stop deriving (Eq, Ord, Show)

fromTuple :: (Int, String) -> Status
fromTuple = uncurry Opening

count :: Status -> Int
count (Opening x _) = x
count Stop = 260
count Start = 0

isRedundant :: Set.Set String -> Status -> Bool
isRedundant s (Opening _ v) = Set.member v s
isRedundant _ _ = False

type ValveGraph = Map.Map String [(Int, String)]

tick :: ValveGraph -> Status -> (Maybe String, [Status])
tick graph Start = (Nothing,) . map  (fromTuple . liftFst pred) . Map.findWithDefault [] "AA" $ graph
tick _ Stop = (Nothing, [Stop])
tick graph (Opening n t)
    | n > 0 = (Nothing, [Opening (n-1) t])
    | otherwise = (Just t,) . (Stop:) . map fromTuple . Map.findWithDefault [] t $ graph

data State = State Status Status (Set.Set String) deriving (Eq, Ord, Show)
    -- { me :: Status
    -- , elephant :: Status
    -- , openValves :: Set.Set String
    -- }

rectify :: State -> State 
rectify s@(State left right open)
    | count right < count left = State right left open
    | otherwise = s 



colliding :: Set.Set String -> State -> Bool
colliding targets (State (Opening _ me) (Opening _ elephant) open) 
    | 1 == Set.size (Set.difference targets open) = False
    | otherwise = me == elephant
colliding _ _ = False

neighbors :: ValveGraph -> State -> [State]
neighbors graph (State me elephant open) = map rectify potential
    where potential = [State me' elephant' open' | me' <- filter valuable myTicks,
                                                   elephant' <- filter valuable elephantTicks]
          valuable = not . isRedundant open'
          open' = Set.union open . Set.fromList . catMaybes $ [iOpened, elephantOpened]
          (iOpened, myTicks) = tick graph me
          (elephantOpened, elephantTicks) = tick graph elephant

cost :: [Valve] -> State -> Integer
cost valves (State _ _ open) = sum . closedFlows valves $ open

sumPairs :: Num a => [a] -> [a]
sumPairs [] = []
sumPairs [x] = [x]
sumPairs (first:second:rest) = first + second : sumPairs rest

optimalOpens :: [Valve] -> State -> Integer
optimalOpens valves (State me elephant open) = wait * currCost + sum (tail steps)
    where wait = succ . toInteger $ min (count me) (count elephant)
          steps = scanl (-) currCost rates
          currCost = sum rates
          rates = sumPairs . reverse . sort . closedFlows valves $ open

maximizeFlowWithElephant :: [Valve] -> Result Int
maximizeFlowWithElephant valves = (optimal -) . fst <$> path
    where optimal = (26*) . fromInteger . sum . map flowRate $ valves
          path = maybeToRight "No Path Found" $ AStar.weighted heuristic nexts (State Start Start Set.empty)
          heuristic = fromInteger . optimalOpens valves
          nexts s = map (AStar.Step stepCost) . filter (not . colliding targets) . neighbors lookups $ s
            where stepCost = fromInteger $ cost valves s
          targets = Set.fromList . map snd . Map.findWithDefault [] "AA" $ lookups
          lookups = buildWorkingValveMap valves

-- | Solution for Day Sixteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseValve) maximizeFlowWithElephant
