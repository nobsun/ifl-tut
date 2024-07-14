module TIM.Mark1gc.GC 
    ( gc
    ) where

import Heap
import Iseq
import Stack

import TIM.Mark1gc.Code
import TIM.Mark1gc.Frame
import TIM.Mark1gc.State
import TIM.Mark1gc.PPrint

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

gc :: (?sz :: Int, ?th :: Int) => TimState -> TimState
gc state = case evacuateFromFramePtr (state.heap, hInitial) state.frame of
    ((from1, to1), fptr1) -> case evacuateFromStack (from1, to1) state.stack of
        ((from2, to2), stack2) -> case evacuateFromDump (from2, to2) state.dump of
            ((from3, to3), dump3) -> case scavange from3 to3 of
                to4 -> trace (gcPrint state.heap from1 to1 from2 to2 from3 to3 to4) $
                       state { frame = fptr1
                             , stack = stack2
                             , dump  = dump3
                             , heap  = to4
                             , stats = statIncGcCount state.stats 
                             }
                      

evacuateFromFramePtr :: (TimHeap, TimHeap) -> FramePtr -> ((TimHeap, TimHeap), FramePtr)
evacuateFromFramePtr (from, to) fptr = undefined

evacuateFromStack :: (TimHeap, TimHeap) -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateFromStack (from, to) stack = undefined

evacuateFromDump :: (TimHeap, TimHeap) -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateFromDump (from, to) dump = undefined

scavange :: TimHeap -> TimHeap -> TimHeap
scavange from to = undefined

gcPrint :: TimHeap 
        -> TimHeap -> TimHeap -> TimHeap -> TimHeap -> TimHeap -> TimHeap
        -> TimHeap
        -> String
gcPrint f0 f1 t1 f2 t2 f3 t3 t4 
    = iDisplay $ iConcat
    [ iNewline
    , iStr "vvvvvvvvvvvvvvvvvvvv"
    , iNewline
    , iStr "before:"
    , iNewline
    , showHeap f0
    , iNewline
    , iStr "evacuated: from1"
    , iNewline
    , showHeap f1
    , iNewline
    , iStr "evacuated: to1"
    , iNewline
    , showHeap t1
    , iNewline 
    , iStr "evacuated: from2"
    , iNewline
    , showHeap f2
    , iNewline
    , iStr "evacuated: to2"
    , iNewline
    , showHeap t2
    , iNewline 
    , iStr "evacuated: from3"
    , iNewline
    , showHeap f3
    , iNewline
    , iStr "evacuated: to3"
    , iNewline
    , showHeap t3
    , iNewline 
    , iStr "scavenged: after"
    , iNewline
    , showHeap t4
    , iNewline
    , iStr "^^^^^^^^^^^^^^^^^^^^"
    , iNewline
    ]
