module Lib.Solution (
    AdventProblem,
    adventOfCode,
    adventOfCodeConfigurable
)where

import Lib.Types
import Lib.Processing
import System.FilePath

type AdventProblem a = String -> IO ([TestResult], Result a)

adventOfCode :: Answer a => Eq a
             => [TestCase a]
             -> Parser b
             -> Solver b a 
             -> AdventProblem a
adventOfCode testCases parse solve baseDir = (,) <$> testResults <*> result
    where testResults = test parse solve resolvedTestCases
          result = readAndApply (dirname </> "input") $ process parse solve
          resolvedTestCases = map (\(name, res) -> (dirname </> name, res)) testCases
          dirname = baseDir

adventOfCodeConfigurable :: Answer b => Eq b
                         => [ConfigurableTestCase a b]
                         -> a
                         -> Parser c
                         -> ConfigurableSolver a c b
                         -> AdventProblem b
adventOfCodeConfigurable testCases arg parse mkSolver baseDir = (,) <$> testResults <*> result
    where testResults = configurableTest parse mkSolver resolvedTestCases
          result = readAndApply (dirname </> "input") $ process parse (mkSolver arg)
          resolvedTestCases = map (\(name, a, res) -> (dirname </> name, a, res)) testCases
          dirname = baseDir
