module Main where

import System.Environment
import Gmachine.Mark2.Machine

main :: IO ()
main = putStr . run =<< readFile . head =<< getArgs
