{-|
module: DayEighteen.PartOne
description: Avent of Code 2022 Day Eighteen, Part One
-}
module DayEighteen.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Parsing
import Helpers.Input
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

data Vertex = Vertex Integer Integer Integer deriving (Eq, Ord)

instance Grokkable Vertex where
    fromResult = grok3 Vertex

parseVertex :: String -> Result Vertex
parseVertex = parse $ scanInt ^& "," ^& scanInt ^& "," ^& scanInt

examples :: [(String, Out)]
examples = [("1", 10), ("2", 64)]

neighbors :: Vertex -> [Vertex]
neighbors (Vertex x y z) = cube
    where cube = [Vertex (x + dx) (y + dy) (z + dz) | dx <- deltas, dy <- deltas, dz <- deltas,
                                                      1 == (sum . map abs) [dx, dy, dz]]
          deltas = [-1, 0, 1]

countSurfaceArea :: [Vertex] -> Int
countSurfaceArea vs = sum . map countExposed $ vs
    where countExposed = length . filter (flip Set.notMember vSet) . neighbors
          vSet = Set.fromList vs

-- | Solution for Day Eighteen, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseVertex) (Right . countSurfaceArea)
