{-|
module: DaySeven.PartOne
description: Avent of Code 2022 Day Seven, Part One
-}
module DaySeven.PartOne(Out, solution, ResolvedFile (..), tree, getSize, parseSession) where

import Lib.Solution
import Lib.Types
import qualified Data.Map.Strict as Map
import Data.List(stripPrefix)
import Data.List.Split(splitOn)
import Text.Read(readEither)
import Distribution.Simple.Utils(safeInit, safeTail)
import Control.Monad(foldM)

-- | The type of the answer to this problem
type Out = Integer

data File = Dir (Map.Map String File) | Plain Integer deriving Show
data ResolvedFile = ResolvedDir Integer (Map.Map String ResolvedFile) | ResolvedPlain Integer

add :: [String] -> File -> (Map.Map String File) -> Result File
add [] (Dir oldFiles) files = Right . Dir $ Map.union files oldFiles
add (n:rest) (Dir files) toInsert = Dir <$> Map.alterF update n files
    where update Nothing = Left $ n ++ " not found"
          update (Just file) = Just <$> add rest file toInsert
add _ _ _ = Left "Cannot insert into a plain file"

resolve :: File -> ResolvedFile
resolve (Plain size) = ResolvedPlain size
resolve (Dir entries) = ResolvedDir size entries'
    where size = sum $ Map.map getSize entries'
          entries' = Map.map resolve entries

getSize :: ResolvedFile -> Integer
getSize (ResolvedPlain size) = size
getSize (ResolvedDir size _) = size

parseCommand :: ([String], File) -> [String] -> Result ([String], File)
parseCommand (ctx, fs) [cmd] = case stripPrefix "cd " cmd of
    Nothing -> Left $ "Unrecognized Command: " ++ cmd
    (Just "/") -> Right ([], fs)
    (Just "..") -> Right (safeInit ctx, fs)
    (Just n) -> Right (ctx ++ [n], fs)
parseCommand (ctx, fs) (cmd:out) = if cmd == "ls"
        then (ctx,) <$> (add ctx fs =<< fileMap)
        else Left $ "Unrecognized Command with output: " ++ cmd
    where fileMap = Map.fromList <$> (sequence . map parseFile $ out)
parseCommand _ [] = Left $ "Cannot Parse empty Command"

parseFile :: String -> Result (String, File)
parseFile line = case stripPrefix "dir " line of
    (Just n) -> Right (n, Dir Map.empty)
    Nothing -> (safeTail fname,) <$> Plain <$> size
        where size = readEither sizeStr
              (sizeStr, fname) = span (/=' ') line

parseSession :: String -> Result ResolvedFile
parseSession = fmap (resolve . snd) . parse . split
    where parse = foldM parseCommand ([], Dir Map.empty) :: [[String]] -> Result ([String], File)
          split = map lines . splitOn "\n$ " . drop 2 :: String -> [[String]]

tree :: ResolvedFile -> [ResolvedFile]
tree f@(ResolvedPlain _) = [f]
tree f@(ResolvedDir _ entries) = [f] ++ (tree =<< Map.elems entries)

sumFoldersWithLimit :: Integer -> ResolvedFile -> Integer
sumFoldersWithLimit n = sum . filter (<=n) . map getSize . filter isDir . tree
    where isDir (ResolvedDir _ _) = True
          isDir _ = False

examples :: [(String, Out)]
examples = [("1", 95437)]

-- | Solution for Day Seven, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseSession (Right . sumFoldersWithLimit 100000)
