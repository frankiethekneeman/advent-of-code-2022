module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory
import System.FilePath
import GHC.Data.Maybe (orElse)
import Lib.Out
import Lib.Types
import qualified DayOne.PartOne
import qualified DayOne.PartTwo
import qualified DayTwo.PartOne
import qualified DayTwo.PartTwo
import qualified DayThree.PartOne
import qualified DayThree.PartTwo
import qualified DayFour.PartOne
import qualified DayFour.PartTwo
import qualified DayFive.PartOne
import qualified DayFive.PartTwo
import qualified DaySix.PartOne
import qualified DaySix.PartTwo
import qualified DaySeven.PartOne
import qualified DaySeven.PartTwo
import qualified DayEight.PartOne
import qualified DayEight.PartTwo
import qualified DayNine.PartOne
import qualified DayNine.PartTwo
import qualified DayTen.PartOne
import qualified DayTen.PartTwo
import qualified DayEleven.PartOne
import qualified DayEleven.PartTwo
import qualified DayTwelve.PartOne
import qualified DayTwelve.PartTwo
import qualified DayThirteen.PartOne
import qualified DayThirteen.PartTwo
import qualified DayFourteen.PartOne
import qualified DayFourteen.PartTwo
import qualified DayFifteen.PartOne
import qualified DayFifteen.PartTwo
import qualified DaySixteen.PartOne
import qualified DaySixteen.PartTwo
import qualified DaySeventeen.PartOne
import qualified DaySeventeen.PartTwo
import qualified DayEighteen.PartOne
import qualified DayEighteen.PartTwo
import qualified DayNineteen.PartOne
import qualified DayNineteen.PartTwo
import qualified DayTwenty.PartOne
import qualified DayTwenty.PartTwo
import qualified DayTwentyOne.PartOne
import qualified DayTwentyOne.PartTwo
import qualified DayTwentyTwo.PartOne
import qualified DayTwentyTwo.PartTwo
import qualified DayTwentyThree.PartOne
import qualified DayTwentyThree.PartTwo
import qualified DayTwentyFour.PartOne
import qualified DayTwentyFour.PartTwo
import qualified DayTwentyFive.PartOne

main :: IO ()
main = do
    args <- getArgs
    runAoC args


runAoC :: [String] -> IO()
runAoC [day, part] = do
    inputDir <- getInputDir day
    getProblem day part inputDir
runAoC _ = putStrLn "Requires exactly two arguments: Day, then Part"


getProblem :: String -> String -> String -> IO()
getProblem "1" "1" = doDisplay . DayOne.PartOne.solution
getProblem "1" "2" = doDisplay . DayOne.PartTwo.solution
getProblem "2" "1" = doDisplay . DayTwo.PartOne.solution
getProblem "2" "2" = doDisplay . DayTwo.PartTwo.solution
getProblem "3" "1" = doDisplay . DayThree.PartOne.solution
getProblem "3" "2" = doDisplay . DayThree.PartTwo.solution
getProblem "4" "1" = doDisplay . DayFour.PartOne.solution
getProblem "4" "2" = doDisplay . DayFour.PartTwo.solution
getProblem "5" "1" = doDisplay . DayFive.PartOne.solution
getProblem "5" "2" = doDisplay . DayFive.PartTwo.solution
getProblem "6" "1" = doDisplay . DaySix.PartOne.solution
getProblem "6" "2" = doDisplay . DaySix.PartTwo.solution
getProblem "7" "1" = doDisplay . DaySeven.PartOne.solution
getProblem "7" "2" = doDisplay . DaySeven.PartTwo.solution
getProblem "8" "1" = doDisplay . DayEight.PartOne.solution
getProblem "8" "2" = doDisplay . DayEight.PartTwo.solution
getProblem "9" "1" = doDisplay . DayNine.PartOne.solution
getProblem "9" "2" = doDisplay . DayNine.PartTwo.solution
getProblem "10" "1" = doDisplay . DayTen.PartOne.solution
getProblem "10" "2" = doDisplay . DayTen.PartTwo.solution
getProblem "11" "1" = doDisplay . DayEleven.PartOne.solution
getProblem "11" "2" = doDisplay . DayEleven.PartTwo.solution
getProblem "12" "1" = doDisplay . DayTwelve.PartOne.solution
getProblem "12" "2" = doDisplay . DayTwelve.PartTwo.solution
getProblem "13" "1" = doDisplay . DayThirteen.PartOne.solution
getProblem "13" "2" = doDisplay . DayThirteen.PartTwo.solution
getProblem "14" "1" = doDisplay . DayFourteen.PartOne.solution
getProblem "14" "2" = doDisplay . DayFourteen.PartTwo.solution
getProblem "15" "1" = doDisplay . DayFifteen.PartOne.solution
getProblem "15" "2" = doDisplay . DayFifteen.PartTwo.solution
getProblem "16" "1" = doDisplay . DaySixteen.PartOne.solution
getProblem "16" "2" = doDisplay . DaySixteen.PartTwo.solution
getProblem "17" "1" = doDisplay . DaySeventeen.PartOne.solution
getProblem "17" "2" = doDisplay . DaySeventeen.PartTwo.solution
getProblem "18" "1" = doDisplay . DayEighteen.PartOne.solution
getProblem "18" "2" = doDisplay . DayEighteen.PartTwo.solution
getProblem "19" "1" = doDisplay . DayNineteen.PartOne.solution
getProblem "19" "2" = doDisplay . DayNineteen.PartTwo.solution
getProblem "20" "1" = doDisplay . DayTwenty.PartOne.solution
getProblem "20" "2" = doDisplay . DayTwenty.PartTwo.solution
getProblem "21" "1" = doDisplay . DayTwentyOne.PartOne.solution
getProblem "21" "2" = doDisplay . DayTwentyOne.PartTwo.solution
getProblem "22" "1" = doDisplay . DayTwentyTwo.PartOne.solution
getProblem "22" "2" = doDisplay . DayTwentyTwo.PartTwo.solution
getProblem "23" "1" = doDisplay . DayTwentyThree.PartOne.solution
getProblem "23" "2" = doDisplay . DayTwentyThree.PartTwo.solution
getProblem "24" "1" = doDisplay . DayTwentyFour.PartOne.solution
getProblem "24" "2" = doDisplay . DayTwentyFour.PartTwo.solution
getProblem "25" "1" = doDisplay . DayTwentyFive.PartOne.solution
getProblem "25" "2" = const $ foldl (>>) msg solutions
    where msg = putStrLn "Running all solutions..."
          solutions = map runWithHeaderAndBreak problems
          runWithHeaderAndBreak xs = putStrLn (mkHeader xs) >> runAoC (toList xs) >> putStrLn ""
          problems = [(show day, show part) | day <- [1..24] :: [Int], part <-[1..2]:: [Int]] ++ [("25", "1")]
          mkHeader (day, part) = "Day " ++ day ++ ", Part " ++ part
          toList (l,r) = [l, r]
          
getProblem day part = const . putStrLn $ "No problem for " ++ show (day,part)

doDisplay :: Answer a => IO ([TestResult], Result a) -> IO ()
doDisplay = ((uncurry display) =<<)

getInputDir :: String -> IO String
getInputDir day = combine <$> basePath <*> pure day
    where basePath = orElse <$> env <*> standard
          env = lookupEnv "AOC_INPUT_DIRECTORY"
          standard = (</> "examples") <$> getCurrentDirectory
