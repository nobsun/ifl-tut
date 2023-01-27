module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Gmachine/Mark5/Machine.hs"]
