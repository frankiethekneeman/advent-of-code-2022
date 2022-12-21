{-|
module: DayNine.PartOne
description: Avent of Code 2022 Day Nine, Part One
-}
module DayNine.PartOne(Out, solution, Dir(..), parseInstructions, go, follow ) where

import Lib.Solution
import Lib.Types
import Data.List(genericReplicate)
import qualified Data.Set as Set
import Helpers.Parsing

-- | The type of the answer to this problem
type Out = Int

-- | A direction on the grid: up, down, left, or right
data Dir = U|D|L|R deriving Show

fromChar :: Char -> Result Dir
fromChar 'U' = Right U
fromChar 'D' = Right D
fromChar 'L' = Right L
fromChar 'R' = Right R
fromChar n = Left $ "Unrecognized Direction: " ++ [n]

instance Grokkable [Dir] where
    fromResult r = genericReplicate <$> len <*> dir
        where dir = fromChar =<< get 0 r
              len = get 1 r :: Result Integer

-- | Turn the input into a list of directions to go - expanding multiple steps into individual
-- Directions
parseInstructions :: String -> Result [Dir]
parseInstructions input = concat <$> (mapM (parse (scanChar ^& " " ^& scanInt)) . lines) input

-- | Translate a point one step on the grid.
go :: (Int, Int) -- ^ The point to translate
    -> Dir -- ^ The direction to translate it
    -> (Int, Int)
go (x, y) U = (x, y + 1)
go (x, y) D = (x, y - 1)
go (x, y) L = (x - 1, y)
go (x, y) R = (x + 1, y)

-- | Implement rope following logic.
follow :: (Int, Int) -- ^ The location to move towards (Head of the rope)
    -> (Int, Int) -- ^ The location that is moving (Tail of the rope)
    ->  (Int, Int)
follow (hx, hy) (tx, ty) = (tx + dx, ty + dy)
    where (dx, dy) = case (hx - tx, hy - ty) of
                (0, 0) -> (0, 0)
                (0, n) -> if abs n == 1 then (0, 0) else (0, scaleDown n)
                (n, 0) -> if abs n == 1 then (0, 0) else (scaleDown n, 0)
                (x, y) -> if abs x == 1 && abs y == 1 then (0, 0) else (scaleDown x, scaleDown y)
          scaleDown n = if n > 0 then 1 else -1


move :: ((Int, Int), (Int, Int)) -> Dir -> ((Int, Int), (Int, Int))
move (h, t) d = (h', t')
    where h' = go h d
          t' = follow h t

modelString :: [Dir] -> Int
modelString = Set.size . Set.fromList . map snd . scanl move ((0, 0), (0, 0))

examples :: [(String, Out)]
examples = [("1", 13)]

-- | Solution for Day Nine, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInstructions (Right . modelString)
