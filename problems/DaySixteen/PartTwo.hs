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
import DaySixteen.PartOne(Valve(..), parseValve)
import GHC.Utils.Misc(liftFst)
import Data.Maybe(catMaybes)
import Data.Either.Combinators(maybeToRight)
import Data.List(sort, intercalate)
import qualified Algorithms.AStar as AStar

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("1", 1707)
           , ("2", 312)
           , ("3", 92)
           , ("4", 88)
           , ("5", 264)
           , ("6", 554)
           , ("friend", 2528)
           ]

type EdgeMap = Map.Map String [String]
type WeightedEdgeMap = Map.Map String [(Int, String)]

buildEdgeMap :: [Valve] -> EdgeMap
buildEdgeMap = Map.fromListWith (++) . map edges
    where edges v = (name v, destinations v)

orF :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orF l r x = l x || r x

toWeightedGraph :: [Valve] -> EdgeMap -> WeightedEdgeMap
toWeightedGraph valves edges = Map.mapWithKey getWeights . Map.restrictKeys edges $ valuable
    where valuable = Set.fromList . ("AA":) . map name . filter (orF hasFlow isJunction) $ valves
          hasFlow = (>0) . flowRate
          isJunction = (>2) . length . destinations
          getWeights source dests = map (tracePath source) dests
          tracePath s d
            | Set.member d valuable = (1, d)
            | otherwise = liftFst succ . tracePath d . head . filter (/=s) . Map.findWithDefault [] d $ edges

data Status = Moving Int String | Opening String | Standing String deriving (Eq, Ord, Show)
data State = State Int Status Status (Set.Set String) deriving (Eq, Ord, Show)

tick :: WeightedEdgeMap -> Status -> (Maybe String, [Status])
tick edges status = case status of
        Standing n -> (Nothing, concat . map (snd . tick edges) . moveFrom $ n)
        Opening n -> (Just n, moveFrom n)
        Moving 0 n -> (Nothing, (Opening n):moveFrom n)
        Moving k n -> (Nothing, [Moving (pred k) n])
    where moveFrom node = map (uncurry Moving . liftFst pred) . Map.findWithDefault [] node $ edges

progress :: WeightedEdgeMap -> State -> [State]
progress edges (State e me elephant open) = [State (e + 1) me' elephant' open' | me' <- myTicks,
                                                                                elephant' <- elephantTicks]
    where open' = Set.union open . Set.fromList . catMaybes $ [myOpen, elephantOpen]
          (myOpen, myTicks) = tick edges me
          (elephantOpen, elephantTicks) = tick edges elephant

--countPathsLessThan :: WeightedEdgeMap -> Int -> String -> Int
--countPathsLessThan edges n curr
--        | n < 0 = 0
--        | n == 0 = 1
--        | otherwise = sum . map (uncurry recurse) $ dests
--    where recurse cost d = countPathsLessThan edges (n-cost) d
--          dests = Map.findWithDefault [] curr $ edges

sumPairs :: Num a => [a] -> [a]
sumPairs [] = []
sumPairs [x] = [x]
sumPairs (x1:x2:xs) = x1 + x2 : sumPairs xs

timeToNextOpen :: Set.Set String -> Int -> Status -> Int
timeToNextOpen open minWalk status = if v == "AA" || Set.member v open then minWalk + base else base
    where (base, v) = case status of
              (Opening n) -> (0, n)
              (Standing n) -> (0, n)
              (Moving t n) -> (t, n)

missedFlow :: [Valve] -> State -> [Integer]
missedFlow valves (State _ _ _ open) = map flowRate closedValves
    where closedValves = filter (flip Set.notMember open . name) valves

optimalOpens :: [Valve] -> Int -> State -> Integer
optimalOpens valves minWalk s@(State e me elephant open) = sum . take (26 - e) $ costs
    where costs = scanl (-) curr $ (replicate delay 0) ++ flowChanges
          delay = minimum . map (timeToNextOpen open minWalk) $ [me, elephant]
          curr = sum closedValves
          flowChanges = intercalate (replicate minWalk 0) . map (pure) $ steps
          steps = sumPairs . reverse . sort $ closedValves
          closedValves = missedFlow valves $ s

nonce :: [Valve] -> Result Out
nonce v = (optimal - ) . fst <$> path
    where optimal = (26 *) . fromInteger . sum . map flowRate $ v
          path = maybeToRight "No Path Found" .  AStar.weighted heuristic nexts $ start
          start = State 0 (Standing "AA") (Standing "AA") Set.empty
          heuristic = fromInteger . optimalOpens v minWalk
          minWalk = minimum . map fst . concat . Map.elems $ weightedEdges
          nexts s = map (AStar.Step cost) . progress weightedEdges $ s
            where cost = fromInteger . sum . missedFlow v $ s
          weightedEdges = toWeightedGraph v . buildEdgeMap $ v 
-- | Solution for Day Sixteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseValve) nonce
