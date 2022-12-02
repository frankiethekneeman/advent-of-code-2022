module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory
import System.FilePath
import GHC.Data.Maybe (orElse)
import Lib.Out
import Lib.Types
import qualified DayOne.PartOne
import qualified DayOne.PartTwo

main :: IO ()
main = do
    args <- getArgs
    let problem = getProblem args
    if (length args) == 0
        then putStrLn "At least one argument is required"
        else do
            inputDir <- getInputDir (args !! 0)
            problem inputDir
 
-- evaluate :: Int -> AdventProblem

getProblem :: [String] -> String -> IO()
getProblem ["1", "1"] = doDisplay . DayOne.PartOne.solution
getProblem ["1", "2"] = doDisplay . DayOne.PartTwo.solution

getProblem _ = (\_ -> putStrLn "Please provide as integers the day and the part to run")

doDisplay :: Answer a => IO ([TestResult], Result a) -> IO ()
doDisplay = ((uncurry display) =<<)

getInputDir :: String -> IO String
getInputDir day = combine <$> basePath <*> pure day
    where basePath = orElse <$> env <*> standard
          env = lookupEnv "AOC_INPUT_DIRECTORY"
          standard = (</> "examples") <$> getCurrentDirectory
