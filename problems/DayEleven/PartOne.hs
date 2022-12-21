{-|
module: DayEleven.PartOne
description: Avent of Code 2022 Day Eleven, Part One
-}
module DayEleven.PartOne(Out, solution, Monkey(..), parseMonkeys) where

import Lib.Solution
import Lib.Types
import Control.Monad(join)
import Data.List(genericLength, sort)
import GHC.Utils.Misc(nTimes)
import Helpers.Parsing

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [("1", 10605)]

newtype Worry = Worry Integer
extract :: Worry -> Integer
extract (Worry i) = i

-- | A Monkey!  A horrible creature who is playing with our stuff.
data Monkey = Monkey
    { inspections :: Integer -- ^ the number of inspections this monkey has done
    , identity :: Integer -- ^ the identity of this monkey
    , itemsHeld :: [Integer] -- ^ The items the monkey is currently holding (not inspected)
    , operation :: Integer -> Integer -- ^ Worry function for inspection
    , test :: Integer -> Integer -- ^ Given a worry level, return a throwing target
    , divisor :: Integer -- ^ All the monkeys use divisibiltiy to check targets. This is the divisor
    }

instance Grokkable Monkey where
    fromResult r = Monkey 0 <$> id' <*> itemsHeld' <*> op <*> test' <*> get 4 r
        where id' = get 0 r
              itemsHeld' = map extract <$> get 1 r
              op = join (deriveOperation <$> get 2 r <*> get 3 r)
              test' = mkTest <$> get 4 r <*> get 5 r <*> get 6 r
              mkTest divisor' t f x = if x `mod` divisor' == 0 then t else f

instance Grokkable Worry where
    fromResult = grok Worry

instance Grokkable [Monkey] where
    fromResult = get 0

char2op :: Char -> Result (Integer -> Integer -> Integer)
char2op '*' = Right (*)
char2op '+' = Right (+)
char2op c = Left $ "Unrecoginzed operation: " ++ [c]

deriveOperation :: Char -> Either Integer String -> Result (Integer -> Integer)
deriveOperation c (Right "old") = (\ f x -> f x x) <$> char2op c
deriveOperation c (Left i) = fmap ($ i) (char2op c)
deriveOperation _ (Right s) = Left $ "Unrecognized placeholder: " ++ s

-- | Parse the whole input file into a list of monkeys.
parseMonkeys :: String -> Result [Monkey]
parseMonkeys = parse inputScanner
    where inputScanner = (monkeyScanner ^* "\n\n") ^& ()
          monkeyScanner = "Monkey " ^& scanInt ^& ":\n"
              ^& "  Starting items: " ^& (scanInt ^* ", ") ^& "\n"
              ^& "  Operation: new = old " ^& scanChar ^& " " ^& (scanInt ^| remember "old") ^& "\n"
              ^& "  Test: divisible by " ^& scanInt ^& "\n"
              ^& "    If true: throw to monkey " ^& scanInt ^& "\n"
              ^& "    If false: throw to monkey " ^& scanInt

inspectAll :: Monkey -> (Monkey, [(Integer, Integer)])
inspectAll m = (m {itemsHeld = [], inspections = inspections'}, items)
    where inspections'= inspections m + genericLength (itemsHeld m)
          items = map inspect $ itemsHeld m
          inspect item = (target, item')
            where target = test m item'
                  item' = operation m item `div` 3

give :: Integer -> Monkey -> Monkey
give item m = m{itemsHeld = itemsHeld'}
    where itemsHeld' = item:itemsHeld m

turn :: Integer -> [Monkey] -> [Monkey]
turn i monkeys = foldl put monkeys' (concat thrown)
    where (monkeys', thrown) = unzip $ map maybeInspectAll monkeys :: ([Monkey], [[(Integer, Integer)]])
          maybeInspectAll m = if i == identity m then inspectAll m else (m, [])
          put ms (target, item) = map catch ms
            where catch m' = if target == identity m' then give item m' else m'

doRound :: [Monkey] -> [Monkey]
doRound m = foldl (flip turn) m (map identity m)

solve :: [Monkey] -> Integer
solve = product . take 2 . reverse . sort . map inspections . nTimes 20 doRound

-- | Solution for Day Eleven, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseMonkeys (Right . solve)
