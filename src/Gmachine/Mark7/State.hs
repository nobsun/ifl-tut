{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark7.State
    where

import Language
import Heap
import Stack
import Utils

import Gmachine.Mark7.Code
import Gmachine.Mark7.Node

--

data GmState
    = GmState
    { ctrl    :: [String]
    , output  :: GmOutput
    , code    :: GmCode
    , stack   :: GmStack
    , dump    :: GmDump
    , heap    :: GmHeap
    , globals :: GmGlobals
    , stats   :: GmStats
    , ruleid  :: GmRuleId
    }

--

type GmOutput = String

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
      , (22, "rule (3.22): Unwind NNum -- restore old stack")
      , (23, "rule (3.23): Eval")
      , (24, "rule (3.24): Arithmetic instruction")
      , (25, "rule (3.25): Arithmetic unary")
      , (26, "rule (3.26): Comparison operator")
      , (27, "rule (3.27): Cond for True")
      , (28, "rule (3.28): Cond for False")
      , (29, "rule (3.29): Unwind for Eval any object to WHNF")
      , (30, "rule (3.30): Pack")
      , (31, "rule (3.31): Casejump")
      , (32, "rule (3.32): Split")
      , (33, "rule (3.33): Print for NNum")
      , (34, "rule (3.34): Print for NConstr")
      , (35, "rule (3.35): Unwind NConstr")
      ]
