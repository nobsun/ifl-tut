module Main where

import System.Environment ( getArgs )
import Lifter.Mark4.Lifter ( run )

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; interact . drive . run =<< readFile fp
    }

drive :: ([String] -> [String]) -> String -> String
drive f = unlines . f . lines

{-
λ. ./executing prog/lift4/sample661.ifl
...
lift4: pushglobal: undeclared global: x_0_1
...
-}