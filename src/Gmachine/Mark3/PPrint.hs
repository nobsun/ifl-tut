{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark3.PPrint
    where

import Language
import Iseq
import Heap
import Stack
import Utils

import Gmachine.Mark3.Code
import Gmachine.Mark3.Node
import Gmachine.Mark3.State

showResults :: [GmState] -> [String]
showResults = map iDisplay . iLayn' 0 . mapoid (showState, showStats)

{- --
showResults :: [GmState] -> String
showResults states = iDisplay
    (iConcat [ iStr "Supercombinator definitions"
             , iNewline
             , iInterleave iNewline (map (showSC s) s.globals)
             , iNewline, iNewline, iStr "State transitions", iNewline, iNewline
             ])
    ++
    (unlines $ map iDisplay $ iLayn' 0 $ mapoid (showState, showStats) states)
    where
        s = head states
-- -}
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
    Unwind       -> iStr "Unwind"
    Pushglobal f -> iStr "Pushglobal " `iAppend` iStr f
    Push n       -> iStr "Push "       `iAppend` iNum n
    Pushint n    -> iStr "Pushint "    `iAppend` iNum n
    Mkap         -> iStr "Mkap"
    Slide n      -> iStr "Slide"       `iAppend` iNum n
    Update n     -> iStr "Update"      `iAppend` iNum n
    Pop n        -> iStr "Pop"         `iAppend` iNum n
    Alloc n      -> iStr "Alloc"       `iAppend` iNum n

showState :: GmState -> IseqRep
showState s = iConcat
    [ showStack s            , iNewline
    , showInstructions s.code, iNewline
    ]

showStack :: GmState -> IseqRep
showStack s = iConcat
    [ iStr " Stack:[ "
    , iIndent (iInterleave iNewline (map (showStackItem s) (reverse s.stack.stkItems)))
    , iStr " ]"
    ]

showStackItem :: GmState -> Addr -> IseqRep
showStackItem s a
    = iConcat [ showAddr a, iStr ": "
              , showNode s a (hLookup s.heap a)
              ]

showNode :: GmState -> Addr -> Node -> IseqRep
showNode s a node = case node of
    NNum n      -> iNum n
    NGlobal _ _ -> iConcat [iStr "Global ", iStr v]
        where
            v = head [ n | (n, b) <- s.globals, a == b ]
    NAp a1 a2   -> iConcat [ iStr "Ap ", showAddr a1
                           , iStr " ",   showAddr a2
                           ]
    NInd a      -> iConcat [ iStr "Ind ", showAddr a]
    _           -> error "showNode: unexpected node"

showStats :: GmState -> IseqRep
showStats s
    = iConcat [ iStr "Steps taken = ", iNum s.stats.steps ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))
