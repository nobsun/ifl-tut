{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module TIM.Mark1.PPrint
    where

import Language
import Iseq
import Heap
import Stack
import Utils

import TIM.Mark1.Code
import TIM.Mark1.Frame
import TIM.Mark1.State

showFullResults :: [TimState] -> String
showFullResults = unlines . showResults

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

showSC :: (Name, Code) -> IseqRep
showSC (name, il)
    = iConcat
    [ iStr "Code for ", iStr name, iStr ":", iNewline
    , iStr "   ", showInstructions Full il, iNewline, iNewline
    ]

showState :: TimState -> IseqRep
showState state
    = iConcat
    [ iStr "Code:  ", showInstructions Terse state.code, iNewline
    , showFrame state.heap state.frPtr
    , showStack state.stack
    , showValueStack state.vstack
    , showDump state.dump
    , iNewline
    ]

showFrame :: TimHeap -> FramePtr -> IseqRep
showFrame heap fptr = case fptr of
    FrameNull      -> iStr "Null frame ptr" `iAppend` iNewline
    FrameAddr addr -> iConcat
                    [ iStr "Frame: <"
                    , iIndent (iInterleave iNewline 
                                (map showClosure (fList (hLookup heap addr))))
                    , iStr ">", iNewline
                    ]
    FrameInt n     -> iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showStack :: TimStack -> IseqRep
showStack stack
    = iConcat
    [ iStr "Arg stack: ["
    , iIndent (iInterleave iNewline (map showClosure stack.stkItems ))
    , iStr "]", iNewline
    ]

showValueStack :: TimValueStack -> IseqRep
showValueStack _vstack = iNil

showDump :: TimDump -> IseqRep
showDump _dump = iNil

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

showInstructions :: HowMuchToPrint -> [Instruction] -> IseqRep
showInstructions d il = case d of
    None  -> iStr "{..}"
    Terse -> iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
        where
            instrs = map (showInstruction None) il
            body | length il <= nTerse = instrs
                 | otherwise           = take nTerse instrs ++ [iStr ".."]
    Full  -> iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"  ]
        where
            sep = iStr "," `iAppend` iNewline
            instrs = map (showInstruction Full) il

showInstruction :: HowMuchToPrint -> Instruction -> IseqRep
showInstruction d instr = case instr of
    Take m  -> iStr "Take "  `iAppend` iNum m
    Enter x -> iStr "Enter " `iAppend` showArg d x
    Push x  -> iStr "Push "  `iAppend` showArg d x

showArg :: HowMuchToPrint -> TimAMode -> IseqRep
showArg d am = case am of
    Arg m    -> iStr "Arg "   `iAppend` iNum m
    Code il  -> iStr "Code "  `iAppend` showInstructions d il
    Label s  -> iStr "Label " `iAppend` iStr s
    IntConst n -> iStr "IntConst " `iAppend` iNum n

nTerse :: Int
nTerse = 3


-- showInstructions :: TimCode -> IseqRep
-- showInstructions is
--     = iConcat [ iStr "  Code:{ "
--               , iIndent (iInterleave iNewline (map showInstruction is))
--               , iStr " }", iNewline]

-- showInstruction :: Instruction -> IseqRep
-- showInstruction i = case i of
--     Slide n      -> iStr "Slide "      `iAppend` iNum n
--     Alloc n      -> iStr "Alloc "      `iAppend` iNum n
--     Update n     -> iStr "Update "     `iAppend` iNum n
--     Pop n        -> iStr "Pop "        `iAppend` iNum n
--     Unwind       -> iStr "Unwind"
--     PushGlobal f -> iStr "PushGlobal " `iAppend` iStr f
--     PushInt n    -> iStr "PushInt "    `iAppend` iNum n
--     Push n       -> iStr "Push "       `iAppend` iNum n
--     Mkap         -> iStr "Mkap"
--     Eval         -> iStr "Eval"
--     Add          -> iStr "Add"
--     Sub          -> iStr "Sub"
--     Mul          -> iStr "Mul"
--     Div          -> iStr "Div"
--     Neg          -> iStr "Neg"
--     Eq           -> iStr "Eq"
--     Ne           -> iStr "Ne"
--     Lt           -> iStr "Lt"
--     Le           -> iStr "Le"
--     Gt           -> iStr "Gt"
--     Ge           -> iStr "Ge"
--     Cond i1 i2
--         -> iConcat
--          [ iStr "Cond [2: ", shortShowInstructions 2 i1
--          , iStr ", 1: ",     shortShowInstructions 2 i2
--          , iStr "]"]
--     Pack t a     
--         -> iConcat
--          [ iStr "Pack ", iNum t, iNum a ]
--     CaseJump nis
--         -> iConcat
--          [ iStr "CaseJump ", showAlts nis ]
--     Split n 
--         -> iConcat
--          [ iStr "Split ", iNum n]
--     Print
--         -> iStr "Print"

-- showAlts :: [(Int, TimCode)] -> IseqRep
-- showAlts nis
--     = iConcat
--     [ iStr "["
--     , iInterleave (iStr ", ") (map showLabels nis)
--     , iStr "]"
--     ]
--     where
--         showLabels (tag, code)
--             = iConcat [ iNum tag
--                       , iStr ": "
--                       , shortShowInstructions 2 code
--                       ]

-- showState :: TimState -> IseqRep
-- showState = undefined
-- showState s = iConcat
--     [ showOutput s           , iNewline
--     , showStack s            , iNewline
--     , showDump s             , iNewline
--     , showInstructions s.code
--     ]

-- showOutput :: TimState -> IseqRep
-- showOutput s = iConcat [iStr "Output: \"", iStr s.output, iStr "\""]

-- showStack :: TimState -> IseqRep
-- showStack s = iConcat
--     [ iStr " Stack:[ "
--     , iIndent (iInterleave iNewline (map (showStackItem s) (reverse s.stack.stkItems)))
--     , iStr " ]"
--     ]

-- showStackItem :: TimState -> Addr -> IseqRep
-- showStackItem s a
--     = iConcat [ showAddr a, iStr ": "
--               , showNode s a (hLookup s.heap a)
--               ]

-- showNode :: TimState -> Addr -> Node -> IseqRep
-- showNode s addr node = case node of
--     NNum n       -> iNum n
--     NGlobal _ _  -> iConcat [iStr "Global ", iStr v]
--         where
--             v = head [ n | (n, b) <- s.globals, addr == b ]
--     NAp a1 a2    -> iConcat [ iStr "Ap ", showAddr a1
--                            , iStr " ",   showAddr a2
--                            ]
--     NInd a       -> iConcat [ iStr "Ind ", showAddr a]
--     NConstr t as -> iConcat [ iStr "Cons ", iNum t, iStr " ["
--                             , iInterleave (iStr ", ") (map showAddr as)
--                             , iStr "]"
--                             ]

-- showDump :: TimState -> IseqRep
-- showDump s
--     = iConcat
--     [ iStr "  Dump:[ "
--     , iIndent (iInterleave iNewline
--         (map showDumpItem (reverse (s.dump.stkItems))))
--     , iStr " ]"
--     ]

-- showDumpItem :: GmDumpItem -> IseqRep
-- showDumpItem (code, stack)
--     = iConcat 
--     [ iStr "<"
--     , shortShowInstructions 3 code, iStr ", "
--     , shortShowStack stack,         iStr ">"
--     ]

-- shortShowInstructions :: Int -> GmCode -> IseqRep
-- shortShowInstructions number code
--     = iConcat
--     [ iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}" ]
--     where
--         codes = map showInstruction (take number code)
--         dotcodes
--             | length code > number = codes ++ [iStr "..."]
--             | otherwise            = codes

-- shortShowStack :: GmStack -> IseqRep
-- shortShowStack stack
--     =  iConcat
--     [ iStr "["
--     , iInterleave (iStr ", ") (map showAddr stack.stkItems)
--     , iStr "]"
--     ]

-- showStats :: TimState -> IseqRep
-- showStats s 
--     = iConcat [ iStr "Steps taken = ", iNum s.stats.steps ]

-- showAddr :: Addr -> IseqRep
-- showAddr addr = iStr ('#' : show addr)

-- showFWAddr :: Addr -> IseqRep
-- showFWAddr addr = iStr (rjustify 4 (show addr))
