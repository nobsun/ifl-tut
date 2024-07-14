{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module TIM.Mark1gc.State
    where

import Control.Arrow

import Language
import Heap
import Stack
import Utils

import TIM.Mark1gc.Code
import TIM.Mark1gc.Frame

--

data TimState
    = TimState
    { ctrl      :: [String]      -- ^ Interactive control sequences
    , code      :: Code          -- ^ The current instruction stream
    , frame     :: FramePtr      -- ^ Address of current frame
    , stack     :: TimStack      -- ^ Stack of arguments
    , vstack    :: TimValueStack -- ^ Value stack
    , dump      :: TimDump       -- ^ Dump
    , heap      :: TimHeap       -- ^ Heap of frames
    , codestore :: CodeStore     -- ^ Labelled blocks of code
    , stats     :: TimStats      -- ^ Statistics
    , ruleid    :: RuleId        -- ^ Transition rule id
    }

type TimStack = Stack Closure

data TimValueStack = DummyTimValueStack

data TimDump = DummyTimDump

type TimHeap = Heap Frame

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap frame =  second FrameAddr (hAlloc heap frame)

fGet   :: TimHeap -> FramePtr -> Int -> Closure
fGet heap fptr n = case fptr of
    FrameAddr addr -> case hLookup heap addr of
        Frame clos     -> clos !! (n - 1)
        _              -> error "fGet: evacuated Frame"
    _              -> error "fGet: invalid frame pointer"

fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fUpdate heap fptr n clos = case fptr of
    FrameAddr addr -> hUpdate heap addr newFrame
        where
            frame = case hLookup heap addr of
                Frame cs -> cs
                _        -> error "fUpdate: evacuated Frame"
            newFrame = Frame $ take (n - 1) frame ++ clos : drop n frame
    _              -> error "fUpdate: invalid frame pointer"


fList :: Frame -> [Closure]
fList f = case f of
    Frame clos -> clos
    _          -> error "evalucated Frame"

data TimStats = TimStats
    { steps    :: Int
    , extime   :: Int
    , hpallocs :: Int
    , gccount  :: Int
    }

statInitial  :: TimStats
statInitial = TimStats { steps = 0 , extime = 0, hpallocs = 0, gccount = 0 }

statIncSteps :: TimStats -> TimStats
statIncSteps s = s { steps = succ s.steps }

statIncExtime :: TimStats -> TimStats
statIncExtime s = s { extime = succ s.extime }

statIncHpAllocs :: Int -> TimStats -> TimStats
statIncHpAllocs n s = s { hpallocs = n + s.hpallocs }

statIncGcCount :: TimStats -> TimStats
statIncGcCount s = s { gccount = succ s.gccount }

type RuleId = Int
