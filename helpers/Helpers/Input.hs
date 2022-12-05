{- |
    Module: Helpers.Input
    Description: Helper functions to make parsing input easier.

    Chiefly concerned with chunking up the input in a way that frees you from
    having to directly confront monadic composition until you're ready.  You
    will see "Monad" sprinkled about in this documentation, but you don't need
    to understand it.  Wherever you see "Monad" you can just substitute the
    "Result" type.
-}
module Helpers.Input where

{- |
  This will break up the input a line at a time and run your parser against each
  line in turn.  Use this method if your parsing is guaranteed, such as if
  each line is a name or something.  If there's any error possible, return an
  Either String a and use `lineByLineM` instead.
-}
lineByLine :: (String -> a) -- ^ A parser for a single line of input
    -> String               -- ^ You will not provide this, the framework will, it is input
    -> [a]
lineByLine f = map f . lines

{- |
  Monadic version of lineByLine.  It will also run your parser against each
  line, but it will gather it into a big Result at the end, which is
  necessary for the adventOfCode family of functions.
-}
lineByLineM :: Monad m
    => (String -> m a) -- ^ Your parser, which returns a Result
    -> String
    -> m [a]
lineByLineM f = sequence . lineByLine f
