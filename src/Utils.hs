{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils
    ( space
    ) where

space :: Int -> String
space = (`replicate` ' ')