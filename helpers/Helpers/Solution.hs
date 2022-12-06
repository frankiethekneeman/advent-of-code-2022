{- |
    Module: Helpers.Solution
    Description: Helper functions to make the framework do work.
-}

module Helpers.Solution where

{-|
Short for "not yet implemented".  This can be used in either the parser or the
solution function in the main adventOfCode Functions.
-}
nyi :: String -> a -> Either String b
nyi name _ = Left $ name ++ " Not Yet Implemented"
