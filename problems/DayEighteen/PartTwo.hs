{-|
module: DayEighteen.PartTwo
description: Avent of Code 2022 Day Eighteen, Part Two
-}
module DayEighteen.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Input
import DayEighteen.PartOne(Vertex(..), parseVertex, neighbors)
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

data BoundingBox = BoundingBox (Integer, Integer) (Integer, Integer) (Integer, Integer)

examples :: [(String, Out)]
examples = [("1", 10), ("2", 58)]

within :: BoundingBox -> Vertex -> Bool
within (BoundingBox (xmin, xmax) (ymin, ymax) (zmin, zmax)) (Vertex x y z) = bounded
    where bounded = boundedX && boundedY && boundedZ
          boundedX = xmin <= x && x <= xmax
          boundedY = ymin <= y && y <= ymax
          boundedZ = zmin <= z && z <= zmax

outside :: BoundingBox -> Set.Set Vertex -> Set.Set Vertex
outside box occupied = fst . head . dropWhile (uncurry (/=)) . (zip <*> tail) $ expansions
    where expansions = iterate (cull . expand) (faces box)
          cull = Set.filter (within box) . flip Set.difference occupied
          expand = Set.fromList . (neighbors =<<) . Set.toList

faces :: BoundingBox -> Set.Set Vertex
faces (BoundingBox (xMin, xMax) (yMin, yMax) (zMin, zMax)) = Set.fromList (xFaces ++ yFaces ++ zFaces)
    where xFaces = [Vertex x y z | x <- [xMin, xMax], y <- [yMin..yMax], z <- [zMin..zMax]]
          yFaces = [Vertex x y z | x <- [xMin..xMax], y <- [yMin, yMax], z <- [zMin..zMax]]
          zFaces = [Vertex x y z | x <- [xMin..xMax], y <- [yMin..yMax], z <- [zMin, zMax]]

bounds :: Set.Set Vertex -> BoundingBox
bounds v = BoundingBox (xMin - 1, xMax + 1) (yMin - 1, yMax + 1) (zMin - 1, zMax + 1)
    where xMin = Set.findMin xs
          xMax = Set.findMax xs
          yMin = Set.findMin ys
          yMax = Set.findMax ys
          zMin = Set.findMin zs
          zMax = Set.findMax zs
          xs = Set.map (\(Vertex x _ _) -> x) v
          ys = Set.map (\(Vertex _ y _) -> y) v
          zs = Set.map (\(Vertex _ _ z) -> z) v

exteriorSurfaceArea :: [Vertex] -> Int
exteriorSurfaceArea vs = sum . map countExposed $ vs
    where countExposed = length . filter (flip Set.member outsidePoints) . neighbors
          outsidePoints = outside box vSet
          box = bounds vSet
          vSet = Set.fromList vs

-- | Solution for Day Eighteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseVertex) (Right . exteriorSurfaceArea)
