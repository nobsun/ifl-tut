module Main where

import System.Environment ( getArgs )
import ParaG.Mark2.Machine ( run )

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines
