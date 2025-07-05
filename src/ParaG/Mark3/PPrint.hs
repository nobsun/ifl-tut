{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ParaG.Mark3.PPrint
    where

import Data.Char
import Data.Maybe
import Language
import Iseq
import Heap
import Stack
import Utils

import ParaG.Mark3.Code
import ParaG.Mark3.Node
import ParaG.Mark3.State

showFullResults :: [PgmState] -> [String]
showFullResults gs = case gs of
    []  -> error "impossible"
    s:_ -> iDisplay (showSCDefns s)
         : showResults gs
         ++ ["all outputs: " 
            ++ "(" 
            ++ unwords ( filter (all isDigit) 
                       $ filter (not . null) 
                       $ map (\ ps -> ps.pgmGlobal.output) gs)
            ++ ")"]

showResults :: [PgmState] -> [String]
showResults = map iDisplay . iLayn' 0 . mapoid (showState, showStats')

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs
mapoid _ [] = []

showSCDefns :: PgmState -> IseqRep
showSCDefns state
    = iInterleave iNewline (map (showSC state) defs)
    where
        defs = state.pgmGlobal.globals

showSC :: PgmState -> (Name, Addr) -> IseqRep
showSC s (name, addr)
    = iConcat [ iStr "Code for ", iStr name, iNewline
              , showInstructions code
              ]
    where
        code = case hLookup s.pgmGlobal.heap addr of
            NGlobal _ c -> c
            _           -> error "showSC: not supercombinator"

showInstructions :: GmCode -> IseqRep
showInstructions is
    = iConcat [ iStr "  Code:{ "
              , iIndent (iInterleave iNewline (map showInstruction is))
              , iStr " }", iNewline]

showInstruction :: Instruction -> IseqRep
showInstruction i = case i of
    Unwind       -> iStr "Unwind"
    PushGlobal f -> iStr "PushGlobal " `iAppend` showGlobalMode f
    PushInt n    -> iStr "PushInt "    `iAppend` iNum n
    MkAp         -> iStr "MkAp"
    Push n       -> iStr "Push "       `iAppend` iNum n
    Pop n        -> iStr "Pop "        `iAppend` iNum n
    Update n     -> iStr "Update "     `iAppend` iNum n
    Slide n      -> iStr "Slide "      `iAppend` iNum n
    Alloc n      -> iStr "Alloc "      `iAppend` iNum n
    PushBasic n  -> iStr "PushBasic "  `iAppend` iNum n
    MkBool       -> iStr "MkBool"
    MkInt        -> iStr "MkInt"
    Get          -> iStr "Get"
    Eval         -> iStr "Eval"
    Add          -> iStr "Add"
    Sub          -> iStr "Sub"
    Mul          -> iStr "Mul"
    Div          -> iStr "Div"
    Neg          -> iStr "Neg"
    Eq           -> iStr "Eq"
    Ne           -> iStr "Ne"
    Lt           -> iStr "Lt"
    Le           -> iStr "Le"
    Gt           -> iStr "Gt"
    Ge           -> iStr "Ge"
    And          -> iStr "And"
    Or           -> iStr "Or"
    Not          -> iStr "Not"
    Cond i1 i2
        -> iConcat
         [ iStr "Cond [2: ", shortShowInstructions 2 i1
         , iStr ", 1: ",     shortShowInstructions 2 i2
         , iStr "]"]
    Pack t a     
        -> iConcat
         [ iStr "Pack ", iNum t, iNum a ]
    CaseJump nis
        -> iConcat
         [ iStr "CaseJump ", showAlts nis ]
    Split n 
        -> iConcat
         [ iStr "Split ", iNum n]
    UpdateBool n -> iStr "UpdateBool " `iAppend` iNum n
    UpdateInt n  -> iStr "UpdateInt " `iAppend` iNum n
    Return       -> iStr "Return"
    Print
        -> iStr "Print"
    Par -> iStr "Par"

showGlobalMode :: GmGlobalMode -> IseqRep
showGlobalMode f = case f of
    GlobalLabel name -> iStr name
    GlobalPack tag arity
        -> iConcat [iStr "Pack{",iNum tag,iStr ",",iNum arity,iStr "}"]

showAlts :: [(Int, GmCode)] -> IseqRep
showAlts nis
    = iConcat
    [ iStr "["
    , iInterleave (iStr ", ") (map showLabels nis)
    , iStr "]"
    ]
    where
        showLabels (tag, code)
            = iConcat [ iNum tag
                      , iStr ": "
                      , shortShowInstructions 2 code
                      ]

showState :: PgmState -> IseqRep
showState s = iConcat
    [ showOutput s.pgmGlobal.output, iNewline
    , showSparks s.pgmGlobal.sparks, iNewline
    , iIndent $ iInterleave iNewline 
              $ curry showLocalState s.pgmGlobal <$> s.pgmLocals
    -- , showHeap s.pgmGlobal
    ]

showHeap :: PgmGlobalState -> IseqRep
showHeap global
    = iConcat
    [ iStr "Heap: "
    , iIndent (iInterleave iNewline (phi <$> global.heap.assocs))
    ]
    where
        phi (a,n) = iConcat [iStr "#", iNum a, iStr ": ", showNode global a n]

showSparks :: GmSparks -> IseqRep
showSparks ss
    = iConcat [ iStr "Sparks: "
              , iInterleave (iStr ", ") $ showSpark <$> ss
              ]

showSpark :: Addr -> IseqRep
showSpark a = iStr "#" `iAppend` iNum a


showLocalState :: GmState -> IseqRep
showLocalState gs@(_global, local)
    = iConcat
    [ iStr "Task #", iNum local.taskid, iStr ": "
    , iIndent (iInterleave iNewline
                [ showInstructions local.code
                , showStack gs
                , showDump gs
                , showVStack gs
                , showClock local.clock
                ])
    , iNewline
    ]

showOutput :: GmOutput-> IseqRep
showOutput o = iConcat [iStr "Output: \"", iStr o, iStr "\""]

showStack :: GmState -> IseqRep
showStack (global, local) = iConcat
    [ iStr " Stack:[ "
    , iIndent (iInterleave iNewline items)
    , iStr " ]"
    ]
    where
        items = map (showStackItem global) (reverse local.stack.stkItems)

showStackItem :: PgmGlobalState -> Addr -> IseqRep
showStackItem global a
    = iConcat [ showAddr a, iStr ": "
              , showNode global a (hLookup global.heap a)
              ]

showNode :: PgmGlobalState -> Addr -> Node -> IseqRep
showNode s addr node = case node of
    NNum n       -> iNum n
    NGlobal _ _  -> iConcat [iStr "Global ", iStr v]
    NAp a1 a2    -> iConcat [ iStr "Ap ", showAddr a1
                            , iStr " ",   showAddr a2
                            ]
    NInd a       -> iConcat [ iStr "Ind ", showAddr a]
    NConstr t as -> iConcat [ iStr "Cons ", iNum t, iStr " ["
                            , iInterleave (iStr ", ") (map showAddr as)
                            , iStr "]"
                            ]
    NLAp a1 a2 tid -> iConcat [ iStr "*Ap ", showAddr a1
                              , iStr " ", showAddr a2
                              , iStr " (locked by task#" ,iNum tid, iStr ")"
                            ]
    NLGlobal _ _ tid -> iConcat [ iStr "*Global ", iStr v
                                , iStr " (locked by task#)", iNum tid, iStr ")"]
    where
        v = list undefined const [ n | (n, b) <- s.globals, addr == b ]

showDump :: GmState -> IseqRep
showDump (_global, local)
    = iConcat
    [ iStr "  Dump:[ "
    , iIndent (iInterleave iNewline
        (map showDumpItem (reverse (local.dump.stkItems))))
    , iStr " ]"
    ]

showDumpItem :: GmDumpItem -> IseqRep
showDumpItem (code, stack, vstack)
    = iConcat 
    [ iStr "<"
    , shortShowInstructions 3 code, iStr ", "
    , shortShowStack stack,         iStr ", "
    , shortShowVStack vstack,       iStr ">"
    ]

shortShowInstructions :: Int -> GmCode -> IseqRep
shortShowInstructions number code
    = iConcat
    [ iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}" ]
    where
        codes = map showInstruction (take number code)
        dotcodes
            | length code > number = codes ++ [iStr "..."]
            | otherwise            = codes

shortShowStack :: GmStack -> IseqRep
shortShowStack stack
    = iConcat
    [ iStr "["
    , iInterleave (iStr ", ") (map showAddr stack.stkItems)
    , iStr "]"
    ]

shortShowVStack :: GmVStack -> IseqRep
shortShowVStack vstack
    = iConcat
    [ iStr "["
    , iInterleave (iStr ", ") (map iNum vstack.stkItems)
    , iStr "]"
    ]

showVStack :: GmState -> IseqRep
showVStack (_global, local)
    = iConcat [iStr "Vstack:[ "
              ,iInterleave (iStr ", ") (map iNum local.vstack.stkItems)
              ,iStr " ]"]

showStats :: PgmState -> IseqRep
showStats s
    = iConcat [ iStr "live durations = ", iStr $ show s.pgmGlobal.stats.durations
              , iNewline
              , iStr "total tasks    = ", iNum $ length s.pgmGlobal.stats.durations
              , iNewline
              , iStr "total clocks   = ", iNum $ sum s.pgmGlobal.stats.durations
              ]

showStats' :: PgmState -> IseqRep
showStats' s
    = iConcat [ showHeap s.pgmGlobal
              , iNewline, iNewline
              , iStr "live durations = ", iStr $ show s.pgmGlobal.stats.durations
              , iNewline
              , iStr "total tasks    = ", iNum $ length s.pgmGlobal.stats.durations
              , iNewline
              , iStr "total clocks   = ", iNum $ sum s.pgmGlobal.stats.durations
              ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))

showClock :: GmClock -> IseqRep
showClock c
    = iConcat
    [ iStr " Clock: "
    , iNum c
    ]