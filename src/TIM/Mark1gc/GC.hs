module TIM.Mark1gc.GC where

import Data.List

import Heap
import Stack
import Utils
import TIM.Mark1gc.Frame
import TIM.Mark1gc.State

gc :: (?sz :: Int, ?th :: Int)
   => TimState -> TimState
-- gc state 
--     | gcFlag = trace "evac fptr" $ case evacuateFromFramePtr state.heap hInitial (state.code, state.frame) of
--         ((from1, to1), fp') -> trace "evac stack" $ case evacuateFromStack from1 to1 state.stack of
--             ((from2, to2), stk') -> trace "evac dump" $ case evacuateFromDump from2 to2 state.dump of
--                 ((from3, to3), dmp') -> state
--                     { frame = fp'
--                     , stack = stk'
--                     , dump  = dmp'
--                     , heap  = scavenge from3 to3
--                     }
--     | otherwise = state

gc state 
    | gcFlag = trace "evac stack" $ case evacuateFromStack state.heap hInitial state.stack of
        ((from1, to1), stk') -> trace "evac dump" $ case evacuateFromDump from1 to1 state.dump of
            ((from2, to2), dump') -> trace "evac fptr" $ case evacuateFromFramePtr from2 to2 (state.code, state.frame) of
                ((from3, to3), fp') -> state
                    { frame = fp'
                    , stack = stk'
                    , dump  = dump'
                    , heap  = scavenge from3 to3
                    }
    | otherwise = state

evacuateAddr :: (?sz :: Int, ?th :: Int)
              => TimHeap -> TimHeap -> Addr -> ((TimHeap, TimHeap), Addr)
evacuateAddr from to a = case hLookup from a of
    Forward a' -> ((from, to), a')
    Frame cs   -> ((from1, to1), a')
        where
            (from1, to1) = undefined
            a' = undefined

evacuateFromFramePtr :: (?sz :: Int, ?th :: Int)
                     => TimHeap -> TimHeap -> Closure -> ((TimHeap, TimHeap), FramePtr)
evacuateFromFramePtr from to (code, fp)
    = case fp of
        FrameAddr a -> case hLookup from a of
            frame@(Frame cs)  -> case fAlloc to frame of
                (to1, fp1) -> case mapAccumL (uncurry evacuateFromFramePtr) (from, to1) cs of
                    ((from2, to2), fps2) -> case fp1 of
                        FrameAddr a3 -> case zipWith (\ (code', _) fp' -> (code', fp')) cs fps2  of
                            cs' -> case hUpdate to2 a3 (Frame cs') of
                                to3 -> ((from2, to3), fp1)
                        _           -> error $ "evacuateFromFramePtr: invalid frame pointer: " ++ show fp1
            Forward _ -> ((from, to), fp)
        FrameInt _n -> ((from, to), fp)
        FrameNull   -> ((from, to), fp)

evacuateFromStack :: (?sz :: Int, ?th :: Int)
                  => TimHeap -> TimHeap -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateFromStack from to stack = case mapAccumL (uncurry evacuateFromFramePtr) (from, to) stack.stkItems of
    (hs, fps) -> (hs, stack { stkItems = zip (aDomain stack.stkItems) fps })

evacuateFromDump :: (?sz :: Int, ?th :: Int)
                 => TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateFromDump from to dump = ((from, to), dump)

scavenge :: (?sz :: Int, ?th :: Int)
         => TimHeap -> TimHeap -> TimHeap
scavenge from to = foldl phi to to.assocs
    where
        phi :: TimHeap -> (Addr, Frame) -> TimHeap
        phi t (a, f) = case f of
            Frame cs -> hUpdate t a (Frame (map conv cs))
                where
                    conv :: Closure -> Closure
                    conv clos@(c,fp) = case fp of
                        FrameAddr old -> case hLookup from old of
                            Forward a'   -> (c, FrameAddr a')
                            _            -> error $ "scavenge: not Forward: " ++ show clos
                        FrameInt _  -> clos
                        FrameNull   -> clos
            Forward _ -> error $ "scavnge: found Forward in new heap: " ++ show f
