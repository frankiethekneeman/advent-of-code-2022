module DayTwo.PartOne where

import Lib.Solution
import Lib.Types
import Control.Monad(join)
import Helpers.Parsing
import Helpers.Input(lineByLineM)
import qualified Data.Map.Strict as Map

tests :: [(String, Int)]
tests = [("1", 15)]

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

scoreMatchup :: RPS -> RPS -> Int
scoreMatchup Rock Paper = 6
scoreMatchup Paper Scissors = 6
scoreMatchup Scissors Rock = 6
scoreMatchup other self | other == self = 3
scoreMatchup _ _ = 0

scoreRound :: Map.Map EncodedPlay RPS -> Round -> Either String Int
scoreRound decoder (Round other encoded) = (+) <$> shapeScore <*> matchupScore
    where shapeScore = scoreShape <$> self
          matchupScore = scoreMatchup other <$> self
          self = case Map.lookup encoded decoder of
                    (Just s) -> Right s
                    Nothing -> Left $ (show encoded) ++ " not decodable"
 
defaultDecoder :: Map.Map EncodedPlay RPS
defaultDecoder = Map.fromList [(X, Rock), (Y, Paper), (Z, Scissors)]

score :: [Round] -> Either String Int
score = fmap (foldr (+) 0) . sequence . map (scoreRound defaultDecoder)


solution :: AdventProblem Int
solution = adventOfCode tests (lineByLineM parseRound) score
