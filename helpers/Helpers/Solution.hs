module Helpers.Solution where

nyi :: String -> a -> Either String b
nyi name _ = Left $ name ++ " Not Yet Implemented"
