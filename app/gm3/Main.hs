module Main where

import System.Environment
import Gmachine.Mark3.Machine

main :: IO ()
main = putStr . run =<< readFile . head =<< getArgs
