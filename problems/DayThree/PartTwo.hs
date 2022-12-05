module DayThree.PartTwo where

import Helpers.Input
import Lib.Solution
import Data.Char
import Control.Applicative(liftA2)
import qualified Data.Set as Set
import Data.List.Split(chunksOf)
examples :: [(String, Int)]
examples = [("1", 70)]

data Pack = Pack String String

halve :: [a] -> Either String ([a], [a])
halve xs = if even l
    then Right $ splitAt (l `div` 2) xs
    else Left "Cannot halve an uneven list"
    where l = length xs
 
lineToPack :: String -> Either String Pack
lineToPack = uncurry Pack <.> halve

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(<.>) f1 f2 arg = f1 <$> f2 arg

items :: Pack -> Set.Set Char
items (Pack first second) = Set.fromList $ first ++ second

intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections [] = Set.empty
intersections [x] = x
intersections (x:xs) = Set.intersection x $ intersections xs

badgePriority :: [Pack] -> Either String Int
badgePriority = sumM . map score . concat . map (Set.elems . intersections) . chunksOf 3 . map items

score :: Char -> Either String Int
score item
    | isLower item = Right $ ord item - ord 'a' + 1
    | isUpper item = Right $ ord item - ord 'A' + 27
    | otherwise = Left $ "Unscorable item: " ++ [item]

sumM :: (Monad m, Foldable f, Num a) => f (m a) -> m a
sumM = foldl (liftA2 (+)) (pure 0)

solution :: AdventProblem Int
solution = adventOfCode examples (lineByLineM lineToPack) badgePriority
