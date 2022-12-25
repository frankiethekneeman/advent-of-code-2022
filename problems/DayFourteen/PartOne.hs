{-|
module: DayFourteen.PartOne
description: Avent of Code 2022 Day Fourteen, Part One
-}
module DayFourteen.PartOne(
    Out,
    solution,
    Point(..),
    getY,
    parseRocks,
    toRocks,
) where

import Lib.Solution
import Lib.Types
import Helpers.Parsing
import Helpers.Input
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Int

-- | A point in space.  Could've used a tuple, but this made using the parsing library easier
data Point = Point Integer Integer deriving (Eq, Ord, Show)

examples :: [(String, Out)]
examples = [("1", 24)]

instance Grokkable Point where
    fromResult = grok2 Point

instance Grokkable [Point] where
    fromResult = get 0

-- | Parse a line of input into a series of points in a bendy rock wall.
parseRocks :: String -> Result [Point]
parseRocks = parse rockVeinScanner
    where rockVeinScanner = point ^* " -> "
          point = scanInt ^& "," ^& scanInt

between :: (Enum a, Ord a) => a -> a -> [a]
between l r = [min l r .. max l r]

-- | Generate the line of rocks between two Points.
generateRocks :: Point -> Point -> [Point]
generateRocks (Point x1 y1) (Point x2 y2)
    | x1 == x2 = map (Point x1) $ between y1 y2
    | y1 == y2 = map (flip Point y1) $ between x1 x2
    | otherwise = []

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

followLine :: [Point] -> Set.Set Point
followLine = Set.fromList . concat . map (uncurry generateRocks) . pairs

addSand :: (Point -> Bool) -> Integer -> Set.Set Point -> Set.Set Point
addSand isAir limit sand = case fall isFree limit (Point 500 0) of
        (Just p) -> Set.insert p sand
        Nothing -> sand
    where isFree x = isAir x && Set.notMember x sand

-- | Turn a list of rock veins into a Set of all the points occupied by rocks.
toRocks :: [[Point]] -> Set.Set Point
toRocks = Set.unions . map followLine

simulateSand :: [[Point]] -> Int
simulateSand veins = fst . head . dropWhile (uncurry (<)) . pairs $ map Set.size sands
    where sands = iterate (addSand isAir maxDepth) Set.empty
          maxDepth = maximum . map getY $ Set.toList rocks
          isAir = flip Set.notMember rocks
          rocks = toRocks veins

down :: Point -> Point
down (Point x y) = Point x (y + 1)

diagLeft :: Point -> Point
diagLeft (Point x y) = Point (x - 1) (y + 1)

-- | Get the vertical depth of the point.
getY :: Point -> Integer
getY (Point _ y) = y

diagRight :: Point -> Point
diagRight (Point x y) = Point (x + 1) (y + 1)

fall :: (Point -> Bool) -> Integer -> Point -> Maybe Point
fall isFree limit point
        | getY point >= limit = Nothing
        | isFree down' = recurse down'
        | isFree diagLeft' = recurse diagLeft'
        | isFree diagRight' = recurse diagRight'
        | otherwise = Just point
    where recurse = fall isFree limit
          down' = down point
          diagLeft' = diagLeft point
          diagRight' = diagRight point

-- | Solution for Day Fourteen, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM parseRocks) (Right . simulateSand)
