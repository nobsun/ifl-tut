module Main where

import System.Environment
import Template.Mark2

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs

