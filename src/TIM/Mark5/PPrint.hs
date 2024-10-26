{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module TIM.Mark5.PPrint
    where

import Language
import Iseq
import Heap
import Stack
import Utils

import TIM.Mark5.Code
import TIM.Mark5.Frame
import TIM.Mark5.State
import qualified Stack as Stk

showFullResults :: [TimState] -> [String]
showFullResults ts = case ts of
    []  -> error "impossible"
    s:_ -> iDisplay (showSCDefns s) : showResults ts

showResults :: [TimState] -> [String]
showResults = map iDisplay . iLayn' 0 . mapoid (showState, showStats)

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs
mapoid _ [] = []

showSCDefns :: TimState -> IseqRep
showSCDefns state
    = iInterleave iNewline (map showSC state.codestore)

showSC :: (Name, CCode) -> IseqRep
showSC (name, il)
    = iConcat
    [ iStr "Code for ", iStr name, iStr ":", iNewline
    , iStr "   ", showInstructions Full il, iNewline, iNewline
    ]

showState :: TimState -> IseqRep
showState state
    = iConcat
    [ iStr "Code:  ", showInstructions Terse state.code, iNewline
    , showFrame state.heap state.frame
    , showStack state.stack
    , showValueStack state.vstack
    , showDump state.dump
    , iNewline
    , showHeap state.heap
    ]

showFrame :: TimHeap -> FramePtr -> IseqRep
showFrame heap fptr = case fptr of
    FrameNull      -> iStr "Null frame ptr" `iAppend` iNewline
    FrameAddr addr -> showFrameEntry (addr, hLookup heap addr)
    FrameInt n     -> iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showFrameEntry :: (Addr, Frame) -> IseqRep
showFrameEntry (addr,frame)
    = iConcat
    [ iStr ("Frame: #" ++ show addr)
    , iNewline
    , iStr  "    "
    , iIndent (iInterleave iNewline 
                (map showClosure (fList frame)))
    , iNewline
    ]

showStack :: TimStack -> IseqRep
showStack stack
    = iConcat
    [ iStr "Arg stack: ["
    , iIndent (iInterleave iNewline (map showClosure stack.stkItems ))
    , iStr "]", iNewline
    ]

showValueStack :: TimValueStack -> IseqRep
showValueStack vstack
    = iStr "VStack: " `iAppend` iStr (show vstack.stkItems)
                      `iAppend` iNewline

showDump :: TimDump -> IseqRep
showDump dump
    = iConcat
    [ iStr "Dump: ["
    , iIndent (iInterleave iNewline (map showDumpItem dump.stkItems))
    , iStr "]", iNewline
    ]

showDumpItem :: TimDumpItem -> IseqRep
showDumpItem item
    = iConcat
    [ iStr "DumpedStack: ["
    , iInterleave (iStr ",") [showFramePtr item.tdframe, iStr (show item.tdindex), showStack item.tdstack ]
    , iStr "]", iNewline
    ]

showClosure :: Closure -> IseqRep
showClosure (i, f)
    = iConcat
    [ iStr "(", showInstructions Terse i, iStr ", "
    , showFramePtr f, iStr ")"
    ]

showFramePtr :: FramePtr -> IseqRep
showFramePtr fptr = case fptr of
    FrameNull   -> iStr "null"
    FrameAddr a -> iStr (show a)
    FrameInt n  -> iStr "int " `iAppend` iNum n

showStats :: TimState -> IseqRep
showStats state
    = iConcat
    [ iStr "Steps take = ", iNum state.stats.steps, iNewline
    , iStr "Execution time = ", iNum state.stats.extime, iNewline
    , iStr "Number of frames allocated = ", iNum state.heap.maxAllocs, iNewline
    , iStr "Heap usage = ", iNum state.stats.hpallocs, iNewline
    , iStr "Max depth of stack = ", iNum state.stack.maxDepth, iNewline
    ]

data HowMuchToPrint
    = Full
    | Terse
    | None

showInstructions :: HowMuchToPrint -> CCode -> IseqRep
showInstructions d il = case d of
    None  -> iStr "{..}"
    Terse -> iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
        where
            instrs = map (showInstruction None) il.code
            body | length il.code <= nTerse = instrs
                 | otherwise           = take nTerse instrs ++ [iStr ".."]
    Full  -> iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"  ]
        where
            sep = iStr "," `iAppend` iNewline
            instrs = map (showInstruction Full) il.code

showInstruction :: HowMuchToPrint -> Instruction -> IseqRep
showInstruction d instr = case instr of
    Take t m  -> iConcat [iStr "Take ", iNum t, iStr " ", iNum m]
    Move n am -> iConcat [iStr "Move ", iNum n, iStr " ", showArg Terse am] 
    Enter x  -> iStr "Enter " `iAppend` showArg d x
    Push x   -> iStr "Push "  `iAppend` showArg d x
    PushV va -> iStr "PushV " `iAppend` showVArg va
    Return   -> iStr "Return"
    Op op    -> iStr (show op)
    Cond i1 i2 -> iConcat [ iStr "Cond "
                          , showInstructions d i1
                          , showInstructions d i2
                          ]
    PushMarker n    -> iStr "PushMarker " `iAppend` iNum n
    UpdateMarkers n -> iStr "UpdateMarkers " `iAppend` iNum n
    Switch is -> iStr "Switch " `iAppend` iNum (length is)
    ReturnConstr t -> iStr "ReturnConstr " `iAppend` iNum t
    
showArg :: HowMuchToPrint -> TimAMode -> IseqRep
showArg d am = case am of
    Arg m    -> iStr "Arg "   `iAppend` iNum m
    Code il  -> iStr "Code "  `iAppend` showInstructions d il
    Label s  -> iStr "Label " `iAppend` iStr s
    IntConst n -> iStr "IntConst " `iAppend` iNum n
    Data i -> iStr "Data " `iAppend` iNum i

nTerse :: Int
nTerse = 3

showVArg :: ValueAMode -> IseqRep
showVArg va = case va of
    FramePtr    -> iStr "FramePtr"
    IntVConst n -> iStr "IntVConst " `iAppend` iNum n

showHeap :: TimHeap -> IseqRep
showHeap heap
    = iConcat
    [ iStr "Heap: "
    , iNewline
    , iStr "    "
    , iIndent (iInterleave iNewline (map showFrameEntry heap.assocs ))
    ]
