module TIM.Mark6.GC
    where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

import Language
import Heap
import qualified Stack as Stk
import Stack hiding (push, pop, npop, discard)
import TIM.Mark6.State
import TIM.Mark6.Frame (FramePtr(..), Frame(..), Closure)
import TIM.Mark6.Code

gcFlag :: Bool
gcFlag = False

gc :: (?sz :: Int, ?th :: Int)
   => TimState -> TimState
gc state 
    | gcFlag    = state
        { frame = frame'
        , stack = stack'
        , dump  = dump'
        , heap  = scavenge from3 to3
        }
    | otherwise = state
    where
        (from0, to0)           = (state.heap, hInitial)
        ((from1, to1), stack') = evacuateFromStack from0 to0 state.stack
        ((from2, to2), dump')  = evacuateFromDump  from1 to1 state.dump
        ((from3, to3), frame') = evacuateFromFramePtr from2 to2 (state.code, state.frame)

evacuateFromStack :: TimHeap -> TimHeap -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateFromStack from to stack
    = case mapAccumL (uncurry evacuateFromFramePtr) (from, to) stack.stkItems of
    ((from',to'), fptrs') -> ((from',to'), stack { stkItems = items' })
        where
            items' = undefined fptrs'

evacuateFromDump :: TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateFromDump from to dump = ((from, to), dump)

evacuateFromFramePtr :: TimHeap -> TimHeap -> (CCode, FramePtr) -> ((TimHeap, TimHeap), FramePtr)
evacuateFromFramePtr from to (ccode, fptr) = case fptr of
    FrameInt _  -> ((from, to), fptr)
    FrameNull   -> ((from, to), fptr)
    FrameAddr a -> case hLookup from a of
        Forward b   -> ((from, to), FrameAddr b)
        

scavenge :: TimHeap -> TimHeap -> TimHeap
scavenge from to = to

{-
type TimStack
    = Stack Closure
    type Closure
        = (CCode, FramePtr)
        data CCode
            = CCode [Int] [Instruction]
        data FramePtr
            = FrameAddr Addr  ★
            | FrameInt  Int
            | FrameNull

type TimHeap
    = Heap Frame
    


-}


{-
-- Gabage Collector (two-space)

gc :: (?sz :: Int, ?th :: Int) => TiState -> TiState
gc state = case evacuateStack state.heap hInitial state.stack of
    ((from1, to1), stack1) -> case evacuateDump from1 to1 state.dump of
        ((from2, to2), dump1)  -> case evacuateGlobals from2 to2 state.globals of
            ((from3, to3), globals1) -> case scavenge from3 to3 of
                to4 ->  trace (gcPrint state.heap from1 to1 from3 to3 to4) $ 
                        state { stack = stack1
                              , dump = dump1
                              , heap = hIncThreshold to4
                              , globals = globals1
                              , stats = incGcCount state.stats
                              }
    where
        gcPrint h0 f1 t1 f3 t3 t4 =  iDisplay $ iConcat [ iNewline
                                                        , iStr "vvvvvvvvvvvvvvvvvvvv"
                                                        , iNewline
                                                        , iStr "before:"
                                                        , iNewline
                                                        , showHeap h0
                                                        , iNewline
                                                        , iStr "evacuated: from1"
                                                        , iNewline
                                                        , showHeap f1
                                                        , iNewline
                                                        , iStr "evacuated: to1"
                                                        , iNewline
                                                        , showHeap t1
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
        -- printHeap heap = iDisplay $ iConcat [ iStr "scavenged: to"
        --                                     , iNewline
        --                                     , showHeap heap
        --                                     , iNewline
        --                                     ]
        hIncThreshold h = if debug  then h { threshold = 2 * h.threshold } else h

evacuateStack :: TiHeap -> TiHeap -> TiStack -> ((TiHeap, TiHeap), TiStack)
evacuateStack from to stack = case mapAccumL evacuateFrom (from, to) stack.stkItems of
    (heaps', addrs') -> (heaps', stack { stkItems = addrs'})

evacuateDump :: TiHeap -> TiHeap -> TiDump -> ((TiHeap, TiHeap), TiDump)
evacuateDump from to dump = ((from, to), dump)

evacuateGlobals :: TiHeap -> TiHeap -> TiGlobals -> ((TiHeap, TiHeap), TiGlobals)
evacuateGlobals from to globals = case unzip globals of
    (names, addrs) -> case mapAccumL evacuateFrom (from, to) addrs of
        ((from1, to1), addrs1) -> ((from1, to1), zip names addrs1)  

evacuateFrom :: (TiHeap, TiHeap) -> Addr -> ((TiHeap, TiHeap), Addr)
evacuateFrom (from, to) a = case trace "eva:01" hLookup from a of
    node -> case node of
        NAp b c -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> case evacuateFrom (from1, to1) b of
                    ((from2, to2), _) -> case evacuateFrom (from2, to2) c of
                        ((from3, to3), _) -> ((from3, to3), a')
        NInd b -> case evacuateFrom (from, to) b of
            ((from1, to1), b') -> ((hUpdate from1 a (NForward b'), to1), b')
        NData _name args -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> case mapAccumL evacuateFrom (from1, to1) args of
                    ((from2, to2), _) -> ((from2, to2), a')
        NForward a' -> ((from, to), a')
        _ -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> ((from1, to1), a')

scavenge :: TiHeap -> TiHeap -> TiHeap
scavenge from to = foldl phi to to.assocs
    where
        phi t (a', n) = case n of
            NAp b c -> case trace "sca:01" hLookup from b of
                NForward b' -> case trace "sca:02" hLookup from c of
                    NForward c'   -> hUpdate t a' (NAp b' c')
                    _ -> error "scavenge: not NForward node"
                _ -> error "scavenge: not NForward node"
            NInd _  -> error "scavenge: NInd"
            NData name args -> hUpdate t a' (NData name (map (unNF . trace "sca:03" hLookup from) args))
            _ -> t
        unNF node = case node of
            NForward fw -> fw
            _           -> error "scavenge: not NForward"
-}
