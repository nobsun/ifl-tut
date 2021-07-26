{- |
module:       Utils
copyright:    (c) Nobuo Yamashita 2021
license:      BSD-3
maintainer:   nobsun@sampou.org
stability:    experimental
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Utils 
  ( space 
  ) where

{- | 半角スペースを指定個数含む文字列 
>>> space 0
""
>>> space 4
"    "
-}
space :: Int -> String
space = (`replicate` ' ')
