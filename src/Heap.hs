{-# LANGUAGE FlexibleInstances #-}
module Heap
    where

import Utils

{- | Heap 
-}
type Addr   = Int
data Heap a = Heap
  { allocs_   :: Int
  , size_     :: Int
  , frees_    :: [Addr]
  , contents_ :: Assoc Addr a
  }

hInitial   :: Heap a
hAlloc     :: Heap a -> a -> (Heap a, Addr)
hUpdate    :: Heap a -> Addr -> a -> Heap a
hFree      :: Heap a -> Addr -> Heap a
hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int
hAllocs    :: Heap a -> Int

hNull      :: Addr
hIsnull    :: Addr -> Bool
showaddr   :: Addr -> String

hInitial = Heap
  { allocs_ = 0
  , size_ = 0
  , frees_  = [1..]
  , contents_ = []
  }
hAlloc     (Heap allocs size (next : free) cts) n   = (Heap (succ allocs) (succ size) free ((next, n) : cts), next)
hAlloc     _                                    _   = error "No space"
hUpdate    (Heap allocs size free          cts) a n = Heap allocs        size         free ((a, n) : remove cts a)
hFree      (Heap allocs size free          cts) a   = Heap allocs (pred size)   (a : free) (remove cts a)
hLookup    (Heap allocs size free          cts) a   = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))
hAddresses (Heap allocs size free          cts)     = [ addr | (addr, node) <- cts ]
hSize      (Heap allocs size free          cts)     = size
hAllocs    (Heap allocs size free          cts)     = allocs

hNull      = 0
hIsnull a  = a == 0
showaddr a = "#" ++ show a

remove :: Assoc Addr a -> Int -> Assoc Addr a
remove [] a = error ("Attempt to update or free noneexistent address #" ++ show a)
remove ((a', n) : cts) a
  | a' == a   = cts
  | otherwise = (a', n) : remove cts a
