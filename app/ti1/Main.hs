module Main where

import System.Environment
import Template.Mark1

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs

