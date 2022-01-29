{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Heap 
    where

import Utils

type Addr = Int

{- | ヒープ
-}
data Heap a = Heap
    { maxAllocs :: Int
    , curAllocs :: Int
    , threshold :: Int
    , frees     :: [Addr]
    , assocs    :: Assoc Addr a
    }

hInitial   :: (?sz :: Int, ?th :: Int) => Heap a 
hAlloc     :: Heap a -> a -> (Heap a, Addr)
hUpdate    :: Heap a -> Addr -> a -> Heap a
hFree      :: Heap a -> Addr -> Heap a
hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]

hInitial = Heap 
    { maxAllocs = 0
    , curAllocs = 0
    , threshold = ?th
    , frees     = [1 .. ?sz]
    , assocs    = []
    }

hAlloc heap node = case heap.frees of
    []   -> error "hAlloc: no space"
    a:rs -> (heap { maxAllocs = heap.maxAllocs `max` succ heap.curAllocs
                  , curAllocs = succ heap.curAllocs
                  , frees     = rs
                  , assocs    = (a, node) : heap.assocs
                  }, a)

hUpdate heap addr node = heap
    { assocs = case break ((addr ==) . fst) heap.assocs of
          (as, _:bs) -> (addr, node) : (as ++ bs)
          _          -> error "hUpdate: no entry"
    }

hFree heap addr = heap
    { curAllocs = pred heap.curAllocs
    , frees     = addr : heap.frees
    , assocs    = case break ((addr ==) . fst) heap.assocs of
        (as,_:bs) -> as ++ bs
        _         -> heap.assocs
    }

hLookup heap addr = aLookup heap.assocs addr (error "hLookup: no entry")

hAddresses heap = aDomain heap.assocs