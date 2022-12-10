{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark5.State
    where

import Language
import Heap
import Stack
import Utils

import Gmachine.Mark5.Code
import Gmachine.Mark5.Node

--

data GmState
    = GmState
    { ctrl    :: [String]
    , code    :: GmCode
    , stack   :: GmStack
    , dump    :: GmDump
    , heap    :: GmHeap
    , globals :: GmGlobals
    , stats   :: GmStats
    , ruleid  :: GmRuleId
    }

--

type GmStack = Stack Addr

--

type GmDump = Stack GmDumpItem
type GmDumpItem = (GmCode, GmStack)

--

type GmHeap = Heap Node

--

type GmGlobals = Assoc Name Addr

--

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

--

type GmEnvironment = Assoc Name Addr


data GmStats
    = GmStats
    { steps :: Int 
    }

statInitial :: GmStats
statInitial = GmStats { steps = 0 }

statIncSteps :: GmStats -> GmStats
statIncSteps stats = stats { steps = succ stats.steps }

--

type GmRuleId = Int

ruleTable :: Assoc GmRuleId String
ruleTable 
    = [ (0, "Initial State")
      , (5, "Rule (3,5): Pushglobal")
      , (6, "Rule (3,6): Pushint")
      , (7, "Rule (3.7): Mkap")
      , (8, "Rule (3.8): Push")
      , (9, "Rule (3.9): Slide")
      , (10, "Rule (3.10): Unwind NNum")
      , (11, "Rule (3.11): Unwind NAp")
      , (12, "rule (3.12): Unwind NGlobal")
      , (13, "rule (3.13): Pushint")
      , (14, "rule (3.14): Pushint")
      , (15, "rule (3.15): Update")
      , (16, "rule (3.16): Pop")
      , (17, "rule (3.17): Unwind NInd")
      , (18, "rule (3.18): Push")
      , (19, "rule (3.19): Unwind NGlobal")
      , (20, "rule (3.20): Alloc")
      , (21, "rule (3.21): Add")
      ]
