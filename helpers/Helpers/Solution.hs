{- |
Module: Helpers.Solution
Description: Helper functions to make the framework do work.
-}

module Helpers.Solution where

import Lib.Types

{-|
Short for "not yet implemented".  This can be used in either the parser or the
solution function in the main adventOfCode Functions.
-}
nyi :: String -> a -> Result b
nyi name _ = Left $ name ++ " Not Yet Implemented"

{-|
Lift a Maybe into a Result with a meaningful error.
-}
maybeToResult :: String -> Maybe a -> Result a
maybeToResult _ (Just x) = Right x
maybeToResult s Nothing = Left s

