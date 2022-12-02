module Lib.Processing where

import Lib.Types

process :: Parser a -> Solver a b -> String -> Result b
process parse solve input = parse input >>= solve

check :: Answer b => Eq b => b -> Parser a -> Solver a b -> String -> Maybe String
check expected parse solve input = toResult $ process parse solve input
    where toResult (Left msg) = Just msg
          toResult (Right actual)
            | actual == expected = Nothing
            | otherwise = Just msg
                where msg = "Expected " ++ (toString expected) ++ ", but got " ++ (toString actual)

readAndApply :: String -> (String -> a) -> IO a
readAndApply fname = (<$> (readFile fname))

test :: Answer b => Eq b
     => Parser a
     -> Solver a b
     -> [TestCase b]
     -> IO [TestResult]
test parse solve = sequence . map exec
    where exec (name, expected) = (name,) <$> result
            where result = readAndApply name $ check expected parse solve

configurableTest :: Answer c => Eq c 
                 => Parser a
                 -> ConfigurableSolver b a c
                 -> [ConfigurableTestCase b c]
                 -> IO [TestResult]
configurableTest parse mkSolver = sequence . map exec
    where exec (name, arg, expected) = (name,) <$> result
            where result = readAndApply name $ check expected parse $ mkSolver arg
