{-|
module: DayThree.PartOne 
description: Advent of Code 2022 Day Three, Part One 
-}
module DayThree.PartOne(solution) where

import Helpers.Input
import Lib.Solution
import Data.Char
import Control.Applicative(liftA2)
import qualified Data.Set as Set
examples :: [(String, Int)]
examples = [("1", 157)]

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

mispackedPriority :: [Pack] -> Either String Int
mispackedPriority = sumM . map score . concat . map (Set.elems . identifyMispacked)

identifyMispacked :: Pack -> Set.Set Char
identifyMispacked (Pack first second) = Set.intersection first' second'
    where first' = Set.fromList first
          second' = Set.fromList second

score :: Char -> Either String Int
score item
    | isLower item = Right $ ord item - ord 'a' + 1
    | isUpper item = Right $ ord item - ord 'A' + 27
    | otherwise = Left $ "Unscorable item: " ++ [item]

sumM :: (Monad m, Foldable f, Num a) => f (m a) -> m a
sumM = foldl (liftA2 (+)) (pure 0)


-- | Solution for Day Three, Part One
solution :: AdventProblem Int
solution = adventOfCode examples (lineByLineM lineToPack) mispackedPriority
