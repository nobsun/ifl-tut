module Main where

import System.Environment
import Template.Mark5a.Machine

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs
