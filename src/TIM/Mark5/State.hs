{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module TIM.Mark5.State
    where

import Control.Arrow

import Language
import Heap
import Stack
import Utils

import TIM.Mark5.Code
import TIM.Mark5.Frame

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
    , stats     :: TimStats      -- ^ Statistics
    , ruleid    :: RuleId        -- ^ Transition rule id
    , output    :: String        -- ^ Print result
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

fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
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
