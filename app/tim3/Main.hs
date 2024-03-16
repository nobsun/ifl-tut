module Main where

import System.Environment
import TIM.Mark3.Machine

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines
