{- |
module:       Utils
copyright:    (c) Nobuo Yamashita 2021
license:      BSD-3
maintainer:   nobsun@sampou.org
stability:    experimental
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Utils where

import Text.Printf

{- | 半角スペースを指定個数含む文字列 
>>> space 0
""
>>> space 4
"    "
-}
space :: Int -> String
space = (`replicate` ' ')

layn :: [String] -> String
layn = unlines . zipWith (printf "%4d)   %s") [1 :: Int ..]

{- | Association List
-}

type Assoc a b = [(a, b)]

aLookup :: Eq a => Assoc a b -> a -> b -> b
aLookup []            k' def = def
aLookup ((k, v) : bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

aDomain :: Assoc a b -> [a]
aDomain alist = [ key | (key, val) <- alist ]

aRange :: Assoc a b -> [b]
aRange alist = [ val | (key, val) <- alist ]

aEmpty :: Assoc a b
aEmpty = []
