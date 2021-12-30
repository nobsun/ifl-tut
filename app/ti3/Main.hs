module Main where

import System.Environment
import Template.Mark3

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs

