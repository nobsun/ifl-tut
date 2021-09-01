{-# LANGUAGE FlexibleInstances #-}
module Heap
    where

import Utils

{- | Heap 
-}
type Addr   = Int
type Heap a = (Int, [Addr], [(Addr, a)])

instance {-# Overlapping #-} Show a => Show (Heap a) where
  show h = case h of
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
showaddr   :: Addr -> String

hInitial                                = (0,         [1..],    [])
hAlloc     (size, next : free, cts) n   = ((succ size, free,     (next, n) : cts), next)
hUpdate    (size, free,        cts) a n = (size,      free,     (a, n) : remove cts a)
hFree      (size, free,        cts) a   = (pred size, a : free, remove cts a)
hLookup    (size, free,        cts) a   = aLookup cts a (error ("can't find node " ++ show a ++ " in heap"))
hAddresses (size, free,        cts)     = [ addr | (addr, node) <- cts ]
hSize      (size, free,        cts)     = size

hNull      = 0
hIsnull a  = a == 0
showaddr a = "#" ++ show a

remove :: [(Addr, a)] -> Int -> [(Addr, a)]
remove [] a = error ("Attempt to update or free noneexistent address #" ++ show a)
remove ((a', n) : cts) a
  | a' == a   = cts
  | otherwise = (a', n) : remove cts a

