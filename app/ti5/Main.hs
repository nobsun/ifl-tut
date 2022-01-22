module Main where

import System.Environment
import Template.Mark5.Machine

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs
