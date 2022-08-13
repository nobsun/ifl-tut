{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5c.PPrint
    where

import Iseq
import Heap
import Stack
import Utils

import Template.Mark5c.State

showResults :: [TiState] -> [String]
showResults = map iDisplay . iLayn' 0 . mapoid (showState, showStats)

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs
mapoid _ _ = error "mapoid: empty list"

showState :: TiState -> IseqRep
showState state = iConcat
    [ showHeap state.heap, iNewline
    , showStack state.heap state.stack, iNewline
    , showDump state.dump, iNewline
    , showOutput state.output, iNewline
    , showRuleId state.ruleid, iNewline
    ]

showHeap :: TiHeap -> IseqRep
showHeap heap = iConcat
    [ iStr "Heap ["
    , iIndent (iInterleave iNewline (map showHeapItem heap.assocs))
    , iStr " ]"
    ]

showHeapItem :: (Addr, Node) -> IseqRep
showHeapItem (addr, node) = iConcat
            [ showFWAddr addr, iStr ": "
            , showNode node
            ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))

showNode :: Node -> IseqRep
showNode node = dispatchNode
    (\ a1 a2 -> iConcat [ iStr "NAp ", showAddr a1, iStr " ", showAddr a2 ])
    (\ name _args _body -> iStr ("NSupercomb " ++ name))
    (\ n -> iStr "NNum " `iAppend` iNum n)
    (\ a -> iStr "NInd " `iAppend` showAddr a)
    (\ name _ -> iStr ("NPrim " ++ name))
    (\ tag args -> iConcat [ iStr ("NData "), iNum tag, iSpace
                           , iInterleave iSpace (map showAddr args)])
    node

showStack :: TiHeap -> TiStack -> IseqRep
showStack heap stack = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem stack.stkItems))
    , iStr " ]", iNewline
    , iStr "Depth ", iNum stack.curDepth
    ]
    where
        showStackItem addr = iConcat
            [ showFWAddr addr, iStr ": "
            , showStkNode heap (hLookup heap addr)
            ]

showDump :: TiDump -> IseqRep
showDump dump = iConcat
    [ iStr ("Dump " ++ show dump.stkItems)
    ]

showStkNode :: TiHeap -> Node -> IseqRep
showStkNode heap node = dispatchNode
    (\ funAddr argAddr -> iConcat [ iStr "NAp ", showFWAddr funAddr
                                  , iStr " ", showFWAddr argAddr, iStr " ("
                                  , showNode (hLookup heap argAddr), iStr ")" ])
    (\ _ _ _ -> showNode node)
    (\ _ -> showNode node)
    (\ _ -> showNode node)
    (\ _ _ -> showNode node)
    (\ _ _ -> showNode node)
    node

showOutput :: TiOutput -> IseqRep
showOutput output = iStr ("Output " ++ show output)

showRuleId :: TiRuleId -> IseqRep
showRuleId rid = iStr desc
    where
        desc = maybe "no description" id $ lookup rid ruleTable

showStats :: TiState -> IseqRep
showStats state = iConcat
    [ iNewline, iStr "Total number of steps = "
    , iNum state.stats.totalSteps
    , iNewline, iStr "             Sc steps = "
    , iNum state.stats.scSteps
    , iNewline, iStr "           Prim steps = "
    , iNum state.stats.primSteps
    , iNewline, iStr "     Allocation count = "
    , iNum state.heap.maxAllocs
    , iNewline, iStr "            Heap size = "
    , iNum state.heap.curAllocs
    , iNewline, iStr "   Max depth of stack = "
    , iNum state.stack.maxDepth
    , iNewline
    ]
