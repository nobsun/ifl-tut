{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark7.PPrint
    where

import Language
import Iseq
import Heap
import Stack
import Utils

import Gmachine.Mark7.Code
import Gmachine.Mark7.Node
import Gmachine.Mark7.State

showResults :: [GmState] -> [String]
showResults = map iDisplay . iLayn' 0 . mapoid (showState, showStats)

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs
mapoid _ [] = []

showSC :: GmState -> (Name, Addr) -> IseqRep
showSC s (name, addr)
    = iConcat [ iStr "Code for ", iStr name, iNewline
              , showInstructions code
              ]
    where
        code = case hLookup s.heap addr of
            NGlobal _ c -> c
            _           -> error "showSC: not supercombinator"

showInstructions :: GmCode -> IseqRep
showInstructions is
    = iConcat [ iStr "  Code:{ "
              , iIndent (iInterleave iNewline (map showInstruction is))
              , iStr " }", iNewline]

showInstruction :: Instruction -> IseqRep
showInstruction i = case i of
    Slide n      -> iStr "Slide "      `iAppend` iNum n
    Alloc n      -> iStr "Alloc "      `iAppend` iNum n
    Update n     -> iStr "Update "     `iAppend` iNum n
    Pop n        -> iStr "Pop "        `iAppend` iNum n
    Unwind       -> iStr "Unwind"
    PushGlobal f -> iStr "PushGlobal " `iAppend` iStr f
    PushInt n    -> iStr "PushInt "    `iAppend` iNum n
    PushBasic n  -> iStr "PushBasic"   `iAppend` iNum n
    Push n       -> iStr "Push "       `iAppend` iNum n
    MkInt        -> iStr "MkInt"
    MkBool       -> iStr "MkBool"
    MkAp         -> iStr "MkAp"
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
    Print
        -> iStr "Print"
    Get -> iStr "Get"

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

showState :: GmState -> IseqRep
showState s = iConcat
    [ showOutput s           , iNewline
    , showStack s            , iNewline
    , showDump s             , iNewline
    , showVStack s           , iNewline
    , showInstructions s.code
    ]

showOutput :: GmState -> IseqRep
showOutput s = iConcat [iStr "Output: \"", iStr s.output, iStr "\""]

showStack :: GmState -> IseqRep
showStack s = iConcat
    [ iStr "  Stack:[ "
    , iIndent (iInterleave iNewline (map (showStackItem s) (reverse s.stack.stkItems)))
    , iStr " ]"
    ]

showStackItem :: GmState -> Addr -> IseqRep
showStackItem s a
    = iConcat [ showAddr a, iStr ": "
              , showNode s a (hLookup s.heap a)
              ]

showNode :: GmState -> Addr -> Node -> IseqRep
showNode s addr node = case node of
    NNum n       -> iNum n
    NGlobal _ _  -> iConcat [iStr "Global ", iStr v]
        where
            v = head [ n | (n, b) <- s.globals, addr == b ]
    NAp a1 a2    -> iConcat [ iStr "Ap ", showAddr a1
                           , iStr " ",   showAddr a2
                           ]
    NInd a       -> iConcat [ iStr "Ind ", showAddr a]
    NConstr t as -> iConcat [ iStr "Cons ", iNum t, iStr " ["
                            , iInterleave (iStr ", ") (map showAddr as)
                            , iStr "]"
                            ]

showDump :: GmState -> IseqRep
showDump s
    = iConcat
    [ iStr "  Dump:[ "
    , iIndent (iInterleave iNewline
        (map showDumpItem (reverse (s.dump.stkItems))))
    , iStr " ]"
    ]

showDumpItem :: GmDumpItem -> IseqRep
showDumpItem (code, stack)
    = iConcat 
    [ iStr "<"
    , shortShowInstructions 3 code, iStr ", "
    , shortShowStack stack,         iStr ">"
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
    =  iConcat
    [ iStr "["
    , iInterleave (iStr ", ") (map showAddr stack.stkItems)
    , iStr "]"
    ]

showVStack :: GmState -> IseqRep
showVStack s 
    = iConcat
    [ iStr "  VStack:[ "
    , iIndent (iInterleave (iStr ", ") (map iNum s.vstack.stkItems))
    , iStr " ]"
    ]

showStats :: GmState -> IseqRep
showStats s
    = iConcat [ iStr "Steps taken = ", iNum s.stats.steps ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))
