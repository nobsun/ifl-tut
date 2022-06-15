{-# LANGUAGE ImplicitParams #-}
module Main where

import System.Environment
import Template.Mark5cp.Machine

main :: IO ()
main = do
    { fp:rs <- getArgs
    ; let ?sz = defaultHeapSize
    ; let ?th = case rs of
                    [] -> defaultThreshold
                    _  -> read (head rs)
    ; interact . drive . run =<< readFile fp
    }

defaultHeapSize :: Int
defaultHeapSize = 2^20

defaultThreshold :: Int
defaultThreshold = 105
