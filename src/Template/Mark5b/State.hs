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
    | length args < 1         = error "primNeg: wrong number of args"
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
        (stack2, dump2) -> state { stack = stack2, dump = dump2 }
    | not (isDataNode arg2Node) = case saveAndPush arg2Addr stack1 state.dump of
        (stack3, dump3) -> state { stack = stack3, dump = dump3 }
    | otherwise                 = doAdminPrimSteps $ setRuleId 17
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
        (stack2, dump2) -> setRuleId 20
                         $ state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ setRuleId 19 $ state { stack = stack1, heap = heap1}
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
        (stack2, dump2) -> state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ state { stack = stack1, heap = heap1 }
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
        (stack2, dump2) -> state { stack = stack2, dump = dump2 }
    | otherwise = doAdminPrimSteps $ state { stack = stack1, heap = heap1 }
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
