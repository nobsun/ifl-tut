{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5.State
    where

import Language
import Heap
import Stack
import Utils

import Template.Mark5.Node

data TiState
    = TiState
    { output  :: TiOutput
    , stack   :: TiStack
    , dump    :: TiDump
    , heap    :: TiHeap
    , globals :: TiGlobals
    , stats   :: TiStats
    , ruleid  :: TiRuleId
    }

type TiOutput  = [Int]

type TiStack   = Stack Addr

type TiDump    = Stack TiStack
initialDump :: TiDump
initialDump = emptyStack

type TiHeap    = Heap Node

type TiGlobals = Assoc Name Addr

data TiStats 
    = TiStats
    { totalSteps :: Int
    , scSteps    :: Int
    , primSteps  :: Int
    }
    deriving Show

initialStats :: TiStats
initialStats = TiStats { totalSteps = 0, scSteps = 0, primSteps = 0 }

incTotalSteps, incScSteps, incPrimSteps :: TiStats -> TiStats
incTotalSteps stats = stats { totalSteps = succ stats.totalSteps }
incScSteps    stats = stats { scSteps    = succ stats.scSteps }
incPrimSteps  stats = stats { primSteps  = succ stats.primSteps }

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = state { stats = f state.stats }

type TiRuleId = Int

setRuleId :: TiRuleId -> TiState -> TiState
setRuleId r state = state { ruleid = r }
