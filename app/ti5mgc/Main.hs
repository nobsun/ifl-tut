{-# LANGUAGE ImplicitParams #-}
module Main where

import System.Environment
import Template.Mark5mgc.Machine

main :: IO ()
main = do
    { fp:_ <- getArgs
    ; let ?sz = defaultHeapSize
    ; let ?th = defaultThreshold
    ; interact . drive . run =<< readFile fp
    }

defaultHeapSize :: Int
defaultHeapSize = 2^(20::Int)

defaultThreshold :: Int
defaultThreshold = 105
