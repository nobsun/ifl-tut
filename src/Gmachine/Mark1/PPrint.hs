{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark1.PPrint
    where

import Language
import Iseq
import Heap
import Stack
import Utils

import Gmachine.Mark1.Code
import Gmachine.Mark1.Node
import Gmachine.Mark1.State

{- -}
showResult :: [GmState] -> String
showResult states = iDisplay
    (iConcat [ iStr "Supercombinator definitions"
             , iNewline
             , iInterleave iNewline (map (showSC s) s.globals)
             , iNewline, iNewline, iStr "State transitions", iNewline, iNewline
             ])
    ++
    (unlines $ map iDisplay $ iLayn' 0 $ mapoid (showState, showStats) states)
    where
        s = head states

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs

showSC :: GmState -> (Name, Addr) -> IseqRep
showSC s (name, addr)
    = iConcat [ iStr "Code for ", iStr name, iNewline
              , showInstructions code, iNewline, iNewline
              ]
    where
        NGlobal arity code = hLookup s.heap addr 

showInstructions :: GmCode -> IseqRep
showInstructions is
    = iConcat [ iStr "  Code:{"
              , iIndent (iInterleave iNewline (map showInstruction is))
              , iStr "}", iNewline]

showInstruction :: Instruction -> IseqRep
showInstruction i = case i of
    Unwind       -> iStr "Unwind"
    Pushglobal f -> iStr "Pushglobal " `iAppend` iStr f
    Push n       -> iStr "Push "       `iAppend` iNum n
    Pushint n    -> iStr "Pushint "    `iAppend` iNum n
    Mkap         -> iStr "Mkap"
    Slide n      -> iStr "Slide "      `iAppend` iNum n

showState :: GmState -> IseqRep
showState s = iConcat
    [ showStack s            , iNewline
    , showInstructions s.code, iNewline
    ]

showStack :: GmState -> IseqRep
showStack s = iConcat
    [ iStr " Stack:["
    , iIndent (iInterleave iNewline (map (showStackItem s) (reverse s.stack.stkItems)))
    , iStr "]"
    ]

showStackItem :: GmState -> Addr -> IseqRep
showStackItem s a
    = iConcat [ showAddr a, iStr ": "
              , showNode s a (hLookup s.heap a)
              ]

showNode :: GmState -> Addr -> Node -> IseqRep
showNode s a node = case node of
    NNum n      -> iNum n
    NGlobal n g -> iConcat [iStr "Global ", iStr v]
        where
            v = head [ n | (n, b) <- s.globals, a == b ]
    NAp a1 a2   -> iConcat [ iStr "Ap ", showAddr a1
                           , iStr " ",   showAddr a2
                           ]

showStats :: GmState -> IseqRep
showStats s
    = iConcat [ iStr "Steps taken = ", iNum s.stats.steps ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))

-- -}
{- --
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
    (\ name args body -> iStr ("NSupercomb " ++ name))
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
-- -}