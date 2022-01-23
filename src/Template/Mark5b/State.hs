{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5b.State
    where

import Language
import Heap
import Stack
import Utils

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

type TiDump    = Stack Int
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

tiFinal :: TiState -> Bool
tiFinal state = isEmptyStack state.stack

-- | Primitive

type Primitive = TiState -> TiState

primitives :: Assoc Name Primitive
primitives = [ ("negate", primNeg)
             , ("+", primArith (+)), ("-", primArith (-))
             , ("*", primArith (+)), ("/", primArith div)
             , ("<", primComp (<)), ("<=", primComp (<=))
             , (">", primComp (>)), (">=", primComp (>=))
             , ("==", primComp (==)), ("/=", primComp (/=))
             , ("if", primIf)
             , ("casePair", primCasePair)
             , ("caseList", primCaseList)
             , ("abort", primAbort)
             , ("stop", primStop)
             , ("print", primPrint)
             ]

primNeg :: TiState -> TiState
primNeg state
    | length args < 1          = error "primNeg: wrong number of args"
    | not (isDataNode argNode) = case saveAndPush argAddr stack1 state.dump of
        (stack2, dump2) -> setRuleId 9
                         $ state { stack = stack2, dump = dump2 }
    | otherwise                = doAdminPrimSteps $ setRuleId 5 
                               $ state { stack = stack1, heap = heap1 }
    where
        args      = take 1 $ getargs state.heap state.stack
        [argAddr] = args
        argNode   = hLookup state.heap argAddr
        NNum argValue = argNode
        (_, stack1) = pop state.stack
        (root, _)   = pop stack1
        heap1 = hUpdate state.heap root (NNum (negate argValue))

primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith op = primDyadic op'
    where
        op' (NNum m) (NNum n) = NNum (m `op` n)

primComp :: (Int -> Int -> Bool) -> TiState -> TiState
primComp op = primDyadic op'
    where
        op' (NNum m) (NNum n)
            | m `op` n  = NData 1 []
            | otherwise = NData 0 []

primDyadic :: (Node -> Node -> Node) -> TiState -> TiState
primDyadic op state 
    | length args < 2 = error "primDyadic: wrong number of args"
    | not (isDataNode arg1Node) = case saveAndPush arg1Addr stack1 state.dump of
        (stack2, dump2) -> setRuleId 16 $ state { stack = stack2, dump = dump2 }
    | not (isDataNode arg2Node) = case saveAndPush arg2Addr stack1 state.dump of
        (stack3, dump3) -> setRuleId 17 $ state { stack = stack3, dump = dump3 }
    | otherwise                 = doAdminPrimSteps $ setRuleId 15
                                $ state { stack = stack1, heap = heap1 }
    where
        args = take 2 $ getargs state.heap state.stack
        [arg1Addr, arg2Addr] = args
        [arg1Node, arg2Node] = map (hLookup state.heap) args
        stack1 = discard 2 state.stack
        (root, _) = pop stack1
        heap1 = hUpdate state.heap root (op arg1Node arg2Node)

primConstr :: Tag -> Arity -> TiState -> TiState
primConstr tag arity state
    | length args < arity = error "primConstr: wrong number of args"
    | otherwise           = setRuleId 10 $ state { stack = stack1, heap = heap1 }
    where
        args = take arity $ getargs state.heap state.stack
        stack1 = discard arity state.stack
        (root,_) = pop stack1
        heap1 = hUpdate state.heap root (NData tag args)

primIf :: TiState -> TiState
primIf state
    | length args < 3 = error "primIf: wrong number of args"
    | not (isDataNode arg1Node) = case saveAndPush arg1Addr stack1 state.dump of
        (stack2, dump2) -> setRuleId 19
                         $ state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ setRuleId 18 $ state { stack = stack1, heap = heap1}
    where
        args = take 3 $ getargs state.heap state.stack
        [arg1Addr, arg2Addr, arg3Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 3 state.stack 
        (root, _) = pop stack1
        result = case arg1Node of
            NData 0 [] -> arg3Addr
            _          -> arg2Addr
        heap1 = hUpdate state.heap root (NInd result)

primCasePair :: TiState -> TiState
primCasePair state
    | length args < 2 = error "primCasePair: wrong number of args"
    | not (isDataNode arg1Node) = case saveAndPush arg1Addr stack1 state.dump of
        (stack2, dump2) -> setRuleId 21 $ state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ setRuleId 20 $ state { stack = stack1, heap = heap1 }
    where
        args = take 2 $ getargs state.heap state.stack
        [arg1Addr, arg2Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 2 state.stack
        (root, _) = pop stack1
        heap1 = case arg1Node of
            NData tag [ft,sd] -> hUpdate heap2 root (NAp addr sd)
                where
                    (heap2 ,addr) = hAlloc state.heap (NAp arg2Addr ft)

primCaseList :: TiState -> TiState
primCaseList state
    | length args < 3 = error "primCaseList: wrong number of args"
    | not (isDataNode arg1Node) = case saveAndPush arg1Addr stack1 state.dump of
        (stack2, dump2) -> setRuleId 23 $ state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ setRuleId 22 $ state { stack = stack1, heap = heap1 }
    where
        args = take 3 $ getargs state.heap state.stack
        [arg1Addr, arg2Addr, arg3Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 3 state.stack
        (root, _) = pop stack1
        heap1 = case arg1Node of
            NData tag cmpnts
                | tag == 0 {- [] -} -> hUpdate state.heap root (NInd arg2Addr)
                | otherwise -> case cmpnts of
                    [hd, tl]  -> hUpdate heap2 root (NAp addr tl)
                        where
                            (heap2, addr) = hAlloc state.heap (NAp arg3Addr hd)
                    _         -> error ("primCaseList: not a cons " ++ show cmpnts)
            _ -> error "not NData node"

primAbort :: TiState -> TiState
primAbort = error "Program abort!"

primStop :: TiState -> TiState
primStop state 
    | not (isEmptyStack state.dump) 
        = error "primStop: dump is not empty"
    | otherwise
        = setRuleId 11 $ state { stack = emptyStack' state.stack }

primPrint :: TiState -> TiState
primPrint state
    | argsLen < 2 = error "primPrint: wrong number of args"
    | not (isEmptyStack state.dump) = error "primPrint: dump is not empty"
    | otherwise = case arg1Node of
        NNum m    -> setRuleId 12 $ state { output = state.output ++ [m]
                                          , stack = push arg2Addr (emptyStack' state.stack)
                                          }
        NData _ _ -> error "primPrint: not a number"
        _         -> case saveAndPush arg1Addr stack1 state.dump of
            (stack2, dump2) -> setRuleId 13 $ state { stack = stack2, dump = dump2 }
    where
        args = take 2 $ getargs state.heap state.stack
        argsLen = length args
        [arg1Addr, arg2Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        NNum arg1Value = arg1Node
        stack1 = discard argsLen state.stack

-- | Node

data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    | NInd Addr
    | NPrim Name Primitive
    | NData Tag [Addr]

dispatchNode :: (Addr -> Addr -> a)               -- ^ NAp
             -> (Name -> [Name] -> CoreExpr -> a) -- ^ NSupercomb
             -> (Int -> a)                        -- ^ NInt
             -> (Addr -> a)                       -- ^ NInd
             -> (Name -> Primitive -> a)          -- ^ NPrim
             -> (Tag -> [Addr] -> a)              -- ^ NData
             -> Node -> a
dispatchNode nap nsupercomb nnum nind nprim ndata node = case node of
    NAp a b                -> nap a b
    NSupercomb f args body -> nsupercomb f args body
    NNum n                 -> nnum n
    NInd a                 -> nind a
    NPrim name prim        -> nprim name prim
    NData tag contents     -> ndata tag contents

isDataNode :: Node -> Bool
isDataNode node = case node of
    NNum _    -> True
    NData _ _ -> True
    _         -> False

doAdminTotalSteps :: TiState -> TiState
doAdminTotalSteps = applyToStats incTotalSteps

doAdminScSteps :: TiState -> TiState
doAdminScSteps = applyToStats incScSteps

doAdminPrimSteps :: TiState -> TiState
doAdminPrimSteps = applyToStats incPrimSteps

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case pop stack of
    (sc, stack1) -> map getarg stack1.stkItems
        where
            getarg addr = case hLookup heap addr of
                NAp _ arg -> arg
                _         -> error "getarg: not application node"

--
emptyStack' :: TiStack -> TiStack
emptyStack' stack = stack { curDepth = 0, stkItems = [] }

saveAndPush :: Addr -> TiStack -> TiDump -> (TiStack, TiDump)
saveAndPush addr stack dump
    = (push addr stack, push stack.curDepth dump)

restore :: TiStack -> TiDump -> (TiStack, TiDump)
restore stack dump
    | isEmptyStack dump = error "restore: dump is empty"
    | otherwise         = case pop dump of
        (sp, dump') 
            -> ( discard (stack.curDepth - sp) stack, dump' )

--
ruleTable :: [(TiRuleId, String)]
ruleTable 
    = [ (1, "Rule (2.1): Unwind a single application node onto the spine stack")
      , (2, "Rule (2.2): Perform a supercombinator reduction")
      , (3, "Rule (2.3): Update the root of redex with an indirection node pointing to the result")
      , (4, "Rule (2.4): Update the indirection node to immediate node")
      , (5, "Rule (2.5): For δ-reduction of negation primitive application with a evaluated argument")
      , (6, "Rule (2.6): For operand evaluation of negation")
      , (7, "Rule (2.7): Restore old stack when operand evaluated")
      , (8, "Rule (2.8): Update the application with a argument points past the indirection")
      , (9, "Rule (2.9): For operand evaluation of negation")
      , (10, "Rule (2.10): For `NPrim (PrimConstr t n)`")
      , (11, "Rule (2.11): For `NPrim Stop`")
      , (12, "Rule (2.12): For `NPrim Print` in case of head element of list evaluated")
      , (13, "Rule (2.13): For `NPrim Print` to evaluate head element of list")
      , (15, "Rule (2.15): Ex 2.21: For δ-reduction of binary operator")
      , (16, "Rule (2.16): Ex 2.21: For left operand evaluation of binary operator")
      , (17, "Rule (2.17): Ex 2.21: For rigth operand evaluation of binary operator")
      , (18, "Rule (2.18): Ex 2.21: For δ-reduction of conditional")
      , (19, "Rule (2.19): Ex 2.21: For evaluation condition of conditional")
      , (20, "Rule (2.20): Ex 2.22: For δ-reduction of casePair")
      , (21, "Rule (2.21): Ex 2.22: For evaluation of 1st arg of casePair")
      , (22, "Rule (2.22): Ex 2.24: For δ-reduction of caseList")
      , (23, "Rule (2.23): Ex 2.24: For evaluation of 1st arg of caseList")
      ]