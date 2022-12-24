{-|
module: DayThirteen.PartOne
description: Avent of Code 2022 Day Thirteen, Part One
-}
module DayThirteen.PartOne(Out, solution, Packet(..), parseInput) where

import Lib.Solution
import Lib.Types
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)
import Distribution.Simple.Utils(safeHead)

-- | The type of the answer to this problem
type Out = Int

-- | Recursive Packet Structure
data Packet = List [Packet] | Number Int deriving Show

instance Eq Packet where
    (Number lhs) == (Number rhs) = lhs == rhs
    (List lhs) == (List rhs) = lhs == rhs
    (List lhs) == rhs = lhs == [rhs] -- rhs is a number
    lhs == rhs = rhs == lhs -- switch to match above

instance Ord Packet where
    compare (Number lhs) (Number rhs) = compare lhs rhs
    compare (List lhs) (List rhs) = fromMaybe lengths pairwise
        where pairwise = safeHead . dropWhile (==EQ) $ zipWith compare lhs rhs
              lengths = compare (length lhs) (length rhs)
    compare lhs rhs = compare (liftToList lhs) (liftToList rhs)
        where liftToList x@(List _) = x
              liftToList x@(Number _) = List [x]

parsePacket :: Maybe [Packet] -> String -> (Packet, String)
parsePacket (Just accum) (']':rest) = (List accum, rest)
parsePacket accum str = case accum of
        (Just l) -> parsePacket (Just $ l ++ [parsed]) rest
        Nothing -> (parsed, rest)
    where (parsed, rest) = case str of
            ('[':r) -> parsePacket (Just []) r
            (',':r) -> parsePacket Nothing r
            s -> (Number n, r)
                where n = read num
                      (num, r) = span (flip elem ['0'..'9']) s

parsePacketLine :: String -> Result Packet
parsePacketLine line = case parsePacket Nothing line of
    (p, "") -> Right p
    (p, "\n") -> Right p -- The last ones
    (_, s) -> Left $ "Did not fully Parse: " ++ show s

parsePacketPair :: String -> Result (Packet, Packet)
parsePacketPair s = (,) <$> firstPacket <*> secondPacket
    where firstPacket = parsePacketLine lhs
          secondPacket = parsePacketLine (tail rhs)
          (lhs, rhs) = span (/='\n') s

-- | Each packet on its own line, with blank lines separating pairs.
parseInput :: String -> Result [(Packet, Packet)]
parseInput = mapM parsePacketPair . splitOn "\n\n"

examples :: [(String, Out)]
examples = [("1", 13)]

sumIndicesOfOrderedPairs :: [(Packet, Packet)] -> Int
sumIndicesOfOrderedPairs = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

-- | Solution for Day Thirteen, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (Right . sumIndicesOfOrderedPairs)
