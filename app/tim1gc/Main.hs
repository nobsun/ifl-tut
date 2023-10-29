module Main where

import System.Environment
import TIM.Mark1gc.Machine

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; let ?sz = defaultHeapSize
    ; let ?th = defaultThreshold
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines
