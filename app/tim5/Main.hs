module Main where

import System.Environment ( getArgs )
import TIM.Mark5.Machine ( run )

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines
