module DayTwo.PartTwo where

import Lib.Solution
import Lib.Types
import Control.Monad(join)
import Helpers.Parsing

tests :: [(String, Int)]
tests = [("1", 12)]

data RPS = Rock | Paper | Scissors deriving (Eq)
data EncodedPlay = X | Y | Z deriving (Show, Ord, Eq)
data Round = Round RPS EncodedPlay

instance Grokkable Round where
    fromResult = join . grok2 fromInput

fromInput :: Char -> Char -> Either String Round
fromInput opponent me = Round <$> parseOpponent opponent <*> parseMe me

parseOpponent :: Char -> Result RPS
parseOpponent 'A' = Right Rock
parseOpponent 'B' = Right Paper
parseOpponent 'C' = Right Scissors
parseOpponent x = Left $ "Unrecognized opponent play: " ++ [x]

parseMe :: Char -> Result EncodedPlay
parseMe 'X' = Right X
parseMe 'Y' = Right Y
parseMe 'Z' = Right Z
parseMe x = Left $ "Unrecognized own play: " ++ [x]

parseRound :: String -> Result Round
parseRound = parse pattern 
    where pattern = scanChar ^& " " ^& scanChar

scoreShape :: RPS -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

toBeat :: RPS -> RPS
toBeat Rock = Paper
toBeat Paper = Scissors
toBeat Scissors = Rock

toLose :: RPS -> RPS
toLose = toBeat . toBeat

scoreMatchup :: RPS -> RPS -> Int
scoreMatchup other self
    | other == self = 3
    | self == toBeat other = 6 
    | otherwise = 0

toPlay :: EncodedPlay -> RPS -> RPS
toPlay X = toLose
toPlay Y = id
toPlay Z = toBeat

scoreRound :: Round -> Int
scoreRound (Round other encoded) = shapeScore + matchupScore
    where shapeScore = scoreShape self
          matchupScore = scoreMatchup other self
          self = toPlay encoded other
 
score :: [Round] -> Int
score = foldr (+) 0 . map scoreRound


-- Move to Helpers.Input
lineByLine :: Monad m => (String -> m a) -> String -> m [a]
lineByLine f = sequence . map f . lines

solution :: AdventProblem Int
solution = adventOfCode tests (lineByLine parseRound) (Right . score)
