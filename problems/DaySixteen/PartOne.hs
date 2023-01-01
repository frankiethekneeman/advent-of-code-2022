{-|
module: DaySixteen.PartOne
description: Avent of Code 2022 Day Sixteen, Part One
-}
module DaySixteen.PartOne(Out, solution, Valve(..), parseValve, buildWorkingValveMap, closedFlows) where

import Lib.Solution
import Lib.Types
import Helpers.Input
import Helpers.Parsing
import Data.Maybe(fromMaybe)
import Data.List(sort, genericTake)
import Data.Either.Combinators(maybeToRight)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Algorithms.AStar as AStar
import GHC.Utils.Misc(liftFst)

-- | The type of the answer to this problem
type Out = Int

-- | A Valve - with name, flow rate and the tunnels leading out.
data Valve = Valve
    { name :: String
    , flowRate :: Integer
    , destinations :: [String]
    }

instance Grokkable Valve where
    fromResult = grok3 Valve

-- | Parse a valve from a line of input
parseValve :: String -> Result Valve
parseValve = parse $ valveData ^& tunnels
    where valveData = "Valve " ^& nameScanner ^& " has flow rate=" ^& scanInt ^& "; "
          tunnels = ("tunnels lead to valves " ^| "tunnel leads to valve ") ^& (nameScanner ^* ", ")
          nameScanner = chomp 2

examples :: [(String, Out)]
examples = [("1", 1651)]

data State = State
    { minutesElapsed :: Integer
    , location :: String
    , openValves :: Set.Set String
    } deriving (Eq, Ord, Show)

missedFlow :: [Valve] -> State -> Integer
missedFlow valves = sum . closedFlows valves . openValves

-- | Calculate the sum of all the closed flows
closedFlows :: [Valve] -> Set.Set String -> [Integer]
closedFlows valves open = map flowRate . filter (flip Set.notMember open . name) $ valves

bestOpens :: [Valve] -> State -> Integer
bestOpens valves (State elapsed _ open) = sum . genericTake (30 - elapsed) $ steps
    where steps = scanl (-) cost rates
          cost = sum rates
          rates = reverse . sort . closedFlows valves $ open

closableValves :: Map.Map String [(Int, String)] -> State -> [(Int, State)]
closableValves distances (State elapsed loc open) = map calcStep openDests
    where calcStep (dist, valve) = (cost, State (elapsed + toInteger cost) valve (Set.insert valve open))
            where cost = dist + 1
          openDests = filter (flip Set.notMember open . snd) dests
          dests = fromMaybe [] $ Map.lookup loc distances

-- | Eliminate non junction and not flowing valves and return a weighted graph.
buildWorkingValveMap :: [Valve] -> Map.Map String [(Int, String)]
buildWorkingValveMap valves = Map.fromListWith (++) dists
    where dists = [(from, [(dist, to)]) | from <- "AA":working,
                                          to <- working,
                                          let dist = routeCost lookups from to]
          working = map name . filter ((>0) . flowRate) $ valves
          lookups = Map.fromList $ map (\v -> (name v, v)) valves

routeCost :: Map.Map String Valve -> String -> String -> Int
routeCost valves from to = maybe maxWalk (pred . length) $ AStar.uniformWeights dist neighbors from
    where maxWalk = Map.size valves
          dist c = if c == to then 0 else 1 -- Don't need a very good heuristic for this tiny map.
          neighbors c = maybe [] destinations $ Map.lookup c valves

maximizeFlow :: [Valve] -> Result Int
maximizeFlow valves = (optimal - ) . fst <$> path
    where optimal = (30*) . fromInteger . sum . map flowRate $ valves
          path = maybeToRight "No Path Found" $ AStar.weighted heuristic nexts (State 0 "AA" Set.empty)
          heuristic = fromInteger . bestOpens valves
          nexts s = map (uncurry AStar.Step . liftFst (cost*)) . closableValves lookups $ s
            where cost = fromInteger $ missedFlow valves s
                  lookups = buildWorkingValveMap valves

-- | Solution for Day Sixteen, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseValve) maximizeFlow
