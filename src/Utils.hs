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

{- | Heap 
-}
type Addr   = Int
type Heap a = (Int, [Addr], [(Addr, a)])

instance {-# Overlapping #-} Show a => Show (Heap a) where
  show = \ case
    (size, free, cts) -> show (size, take 5 free, cts)


hInitial   :: Heap a
hAlloc     :: Heap a -> a -> (Heap a, Addr)
hUpdate    :: Heap a -> Addr -> a -> Heap a
hFree      :: Heap a -> Addr -> Heap a
hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int

hNull      :: Addr
hIsnull    :: Addr -> Bool
showAddr   :: Addr -> String

hInitial                                = (0,         [1..],    [])
hAlloc     (size, next : free, cts) n   = ((succ size, free,     (next, n) : cts), next)
hUpdate    (size, free,        cts) a n = (size,      free,     (a, n) : remove cts a)
hFree      (size, free,        cts) a   = (pred size, a : free, remove cts a)
hLookup    (size, free,        cts) a   = aLookup cts a (error ("can't find node " ++ show a ++ " in heap"))
hAddresses (size, free,        cts)     = [ addr | (addr, node) <- cts ]
hSize      (size, free,        cts)     = size

hNull      = 0
hIsnull a  = a == 0
showAddr a = "#" ++ show a

remove :: [(Addr, a)] -> Int -> [(Addr, a)]
remove [] a = error ("Attempt to update or free noneexistent address #" ++ show a)
remove ((a', n) : cts) a
  | a' == a   = cts
  | otherwise = (a', n) : remove cts a

{- | Association List
-}

type ASSOC a b = [(a, b)]

aLookup :: Eq a => ASSOC a b -> a -> b -> b
aLookup []            k' def = def
aLookup ((k, v) : bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [ key | (key, val) <- alist ]

aRange :: ASSOC a b -> [b]
aRange alist = [ val | (key, val) <- alist ]

aEmpty :: ASSOC a b
aEmpty = []
