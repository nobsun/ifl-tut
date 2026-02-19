module Main where

main :: IO ()
main = interact id

h p q = p * q
f x = let
        g = \ y -> h x x + y
      in g 2 + g 4
mm = f 6 :: Int
