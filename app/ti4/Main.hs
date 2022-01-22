module Main where

import System.Environment
import Template.Mark4

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs
