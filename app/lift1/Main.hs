module Main where

import System.Environment ( getArgs )
import Lifter.Mark1.Lifter ( run, runS )

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines
