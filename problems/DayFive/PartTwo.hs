{-|
module: DayFive.PartTwo
description: Avent of Code 2022 Day Five, Part Two
-}
module DayFive.PartTwo(Out, solution) where


import Lib.Solution
import Lib.Types
import Helpers.Parsing
import Data.List(transpose,genericSplitAt,uncons)
import Control.Applicative(liftA)
import Control.Monad(foldM)
import Data.Maybe(catMaybes, mapMaybe)
import Distribution.Simple.Utils(safeInit,safeTail,safeHead)
import Helpers.Solution(maybeToResult)

-- | The type of the answer to this problem
type Out = String

examples :: [(String, Out)]
examples = [("1", "MCD")]

type Stacks = [[Char]]
data StackLevel = StackLevel [Maybe Char]

unmarshall :: StackLevel -> [Maybe Char]
unmarshall (StackLevel c) = c

instance Grokkable StackLevel where
    fromResult = grok StackLevel

parseStacks :: [String] -> Result Stacks
parseStacks = deMaybe . transpose' .  mapM (fmap unmarshall . parse stackRowScanner)
    where transpose' = fmap transpose
          deMaybe = fmap $ map catMaybes
          stackRowScanner = ((empty ^| full) ^* " ") ^& ()
          empty = "   " ^& indicatedByEmptyList
          full = "[" ^& scanChar ^& "]"

data Instruction = Instruction Integer Integer Integer

instance Grokkable Instruction where
    fromResult = grok3 Instruction
parseInstructions :: [String] -> Result [Instruction]
parseInstructions = mapM (parse insScanner)
    where insScanner = "move " ^& scanInt ^& " from " ^& scanInt ^& " to " ^& scanInt ^& ()

parseDockState :: String -> Result (Stacks, [Instruction])
parseDockState input = (,) <$> stacks <*> instructions
    where stacks = parseStacks stackRows
          instructions = parseInstructions instructionRows
          stackRows = safeInit before
          instructionRows = safeTail after
          (before, after) = span (/="") . lines $ input

getStack :: Integer -> Stacks -> Result ([[Char]], [Char], [[Char]])
getStack n stacks = do
    let (before, rest) = genericSplitAt (n - 1) stacks
    (target, after) <- maybeToResult ("No stack at position " ++ show n) $ uncons rest
    pure (before, target, after)

pop :: Integer -> Integer -> Stacks -> Result ([Char], Stacks)
pop n p stacks = do
    (before, target, after) <- getStack p stacks
    let (popped, target') = genericSplitAt n target
    pure (popped, before ++ target':after)

push :: [Char] -> Integer -> Stacks -> Result Stacks
push c n stacks = do
    (before, target, after) <- getStack n stacks
    pure $ before ++ (c ++ target):after

move :: Integer -> Integer -> Integer -> Stacks -> Result Stacks
move n from to stacks = do
    (popped, stacks') <- pop n from stacks
    push popped to stacks'

executeInstruction :: Instruction -> Stacks -> Result Stacks
executeInstruction (Instruction 0 _ _) s = Right s
executeInstruction (Instruction n from to) s = move n from to s

executeInstructions :: Stacks -> [Instruction] -> Result Stacks
executeInstructions = foldM (flip executeInstruction)

getMessage :: Stacks -> String
getMessage = mapMaybe safeHead

solve :: Stacks -> [Instruction] -> Result String
solve s i = getMessage <$> executeInstructions s i

-- | Solution for Day Five, Part Two
solution :: AdventProblem Out
solution = adventOfCode examples parseDockState (uncurry solve)
