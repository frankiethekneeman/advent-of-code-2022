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

main :: IO ()
main = do
    args <- getArgs
    let problem = getProblem args
    if (length args) == 0
        then putStrLn "At least one argument is required"
        else do
            inputDir <- getInputDir (args !! 0)
            problem inputDir
 
getProblem :: [String] -> String -> IO()
getProblem ["1", "1"] = doDisplay . DayOne.PartOne.solution
getProblem ["1", "2"] = doDisplay . DayOne.PartTwo.solution
getProblem ["2", "1"] = doDisplay . DayTwo.PartOne.solution
getProblem ["2", "2"] = doDisplay . DayTwo.PartTwo.solution
getProblem ["3", "1"] = doDisplay . DayThree.PartOne.solution
getProblem ["3", "2"] = doDisplay . DayThree.PartTwo.solution
getProblem _ = (\_ -> putStrLn "Please provide as integers the day and the part to run")

doDisplay :: Answer a => IO ([TestResult], Result a) -> IO ()
doDisplay = ((uncurry display) =<<)

getInputDir :: String -> IO String
getInputDir day = combine <$> basePath <*> pure day
    where basePath = orElse <$> env <*> standard
          env = lookupEnv "AOC_INPUT_DIRECTORY"
          standard = (</> "examples") <$> getCurrentDirectory
