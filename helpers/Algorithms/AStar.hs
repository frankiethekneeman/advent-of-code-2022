{-|
  DO NOT PROPAGATE.

  This is a generalized version of the AStar Algorithm, which I developed for my own use.
  It should not appear in the actual template version of this Repo.
-}
module Algorithms.AStar (
    Step(..),
    uniformWeights,
    weighted,
    StepsF,
    DistF,
    NeighborsF,
) where

import qualified Data.Set as Set
import qualified DataStructures.MinPQueue as Queue

-- | A minimal step along the graph, with Cost.  Cost, then the new node.
data Step a = Step Int a
data Path a = Path Int [a]

-- | A function that defines the distance from a vertex to the target
type DistF a = a -> Int
-- | Return all the neighbors of a vertex
type NeighborsF a = a -> [a]
-- | Return the steps from a vertex
type StepsF a = a -> [Step a]
type WeightedPaths a = Queue.MinPQueue Int (Path a)


-- | AStar where every step has the same weight (namely, 1).  Returns the shortest path, if one exists
uniformWeights :: Ord a
    => DistF a -- ^ Heuristic for distance to the target node.
    -> NeighborsF a -- ^ Get all (legal) neighbor nodes
    -> a -- ^ Starting node
    -> Maybe [a]
uniformWeights dist neighbors start = snd <$> weightedSearch dist next Set.empty (startPath, Queue.empty)
    where next = map (Step 1) . neighbors
          startPath = Path 0 [start]

-- | AStar where every step may have a different weight.  Returns the lowest weighted path, if one exists.
weighted :: Ord a
    => DistF a -- ^ Heuristic for distance to the target node
    -> StepsF a -- ^ Get all steps, with weight
    -> a -- ^ Starting Node
    -> Maybe (Int, [a])
weighted dist next start = weightedSearch dist next Set.empty (Path 0 [start], Queue.empty)

vertex :: Step a -> a
vertex (Step _ x) = x

weightedSearch :: Ord a => DistF a -> StepsF a -> Set.Set a -> (Path a, WeightedPaths a) -> Maybe (Int, [a])
weightedSearch _ _ _ (Path _ [], _) = Nothing
weightedSearch dist next seen (Path cost path, paths)
    | dist curr == 0 = Just (cost, path)
    | Set.member curr seen = Queue.pop paths >>= weightedSearch dist next seen
    | otherwise = Queue.pop paths' >>= weightedSearch dist next seen'
    where curr = head path
          paths' = Queue.insertAll paths steps
          steps = map prep . filter (not.alreadySeen.vertex) $ next curr
          prep (Step weight v) = (cost + weight + dist v, Path (cost + weight) (v:path))
          alreadySeen = flip Set.member seen'
          seen' = Set.insert curr seen

