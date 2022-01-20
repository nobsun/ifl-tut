module Main where

import System.Environment
import Template.Mark5b.Machine

main :: IO ()
main = putStrLn . run =<< readFile . head =<< getArgs
