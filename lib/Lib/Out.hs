module Lib.Out where

import Lib.Types

display :: Answer a => [TestResult] -> Result a -> IO ()
display tests result = testOutput >> putStrLn testGuard
    where testOutput = mapM_ (putStrLn . showResult) tests :: IO ()
          testGuard = if passing tests then resultMsg else "Please fix issues with tests before attempting on main input"
          resultMsg = case result of
            Left msg -> "There was an error on the main input: " ++ msg
            Right out -> toString out

showResult :: TestResult -> String
showResult (name, Nothing) = "Test " ++ name ++ " passed."
showResult (name, Just msg) = "Test " ++ name ++ " failed: " ++ msg

passing :: [TestResult] -> Bool
passing = foldr (&&) True . map ( pass . snd)
    where pass (Just _) = False
          pass Nothing = True

