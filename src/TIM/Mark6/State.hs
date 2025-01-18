{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module TIM.Mark6.State
    where

import Control.Arrow
import GHC.Stack (HasCallStack)

import Language
import Heap
import Stack
import Utils

import TIM.Mark6.Code
import TIM.Mark6.Frame

--

data TimState
    = TimState
    { ctrl      :: [String]      -- ^ Interactive control sequences
    , code      :: CCode         -- ^ The current instruction stream
    , frame     :: FramePtr      -- ^ Address of current frame
    , dataframe :: FramePtr      -- ^ Address of current data frame
    , stack     :: TimStack      -- ^ Stack of arguments
    , vstack    :: TimValueStack -- ^ Value stack
    , dump      :: TimDump       -- ^ Dump
    , heap      :: TimHeap       -- ^ Heap of frames
    , codestore :: CodeStore     -- ^ Labelled blocks of code
    , symtbl    :: Assoc Name Int -- ^ Symbol table for debug print
    , stats     :: TimStats      -- ^ Statistics
    , ruleid    :: RuleId        -- ^ Transition rule id
    , output    :: Maybe String  -- ^ Print result
    }

type TimStack = Stack Closure

type TimValueStack = Stack Int

type TimDump = Stack TimDumpItem

data TimDumpItem
    = TimDumpItem
    { tdframe :: FramePtr
    , tdindex :: Int
    , tdstack :: TimStack
    }

type TimHeap = Heap Frame

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap frame =  second FrameAddr (hAlloc heap frame)

fGet   :: TimHeap -> FramePtr -> Int -> Closure
fGet heap fptr n = case fptr of
    FrameAddr addr -> case hLookup heap addr of
        Frame cls -> cls !! (n - 1)
        Forward _ -> error "forwarded"
    _              -> error "fGet: invalid frame pointer"

fUpdate :: HasCallStack => TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fUpdate heap fptr n clos = case fptr of
    FrameAddr addr -> hUpdate heap addr newFrame
        where
            frame = case hLookup heap addr of
                Frame cls -> cls
                Forward _ -> error "forwarded"
            newFrame = Frame $ take (n - 1) frame ++ clos : drop n frame
    _              -> error "fUpdate: invalid frame pointer"


fList :: Frame -> [Closure]
fList fr = case fr of
    Frame cls -> cls
    _         -> error "forward"

data TimStats = TimStats
    { steps  :: Int
    , extime :: Int
    , hpallocs :: Int
    }

statInitial  :: TimStats
statInitial = TimStats { steps = 0 , extime = 0, hpallocs = 0 }

statIncSteps :: TimStats -> TimStats
statIncSteps s = s { steps = succ s.steps }

statIncExtime :: TimStats -> TimStats
statIncExtime s = s { extime = succ s.extime }

statIncnExtime :: Int -> TimStats -> TimStats
statIncnExtime n s = s { extime = n + s.extime }

statIncHpAllocs :: Int -> TimStats -> TimStats
statIncHpAllocs n s = s { hpallocs = n + s.hpallocs }

type RuleId = Int

-- type CodeStore = (FramePtr, Assoc Name Int)
type CodeStore = FramePtr

-- codeLookup :: TimHeap -> CodeStore -> Name -> CCode
-- codeLookup heap codestore lab
--     = fst (codeLookup' heap codestore lab)

-- codeLookup' :: TimHeap -> CodeStore -> Name -> Closure
-- codeLookup' heap (fptr, names) lab
--     = fGet heap fptr i
--     where
--         i = aLookup names lab (error "codeLookup fail")
