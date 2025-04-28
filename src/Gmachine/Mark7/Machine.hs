{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gmachine.Mark7.Machine
    where

import Data.Char
import Data.Function
import Data.List
-- import Text.ParserCombinators.ReadP

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import Gmachine.Mark7.Code
import Gmachine.Mark7.Compiler
import Gmachine.Mark7.Node
import Gmachine.Mark7.PPrint
import Gmachine.Mark7.State

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

run :: String -> ([String] -> [String])
run prog inputs 
    = showFullResults 
    $ eval 
    $ setControl inputs
    $ compile 
    $ parse prog

setControl :: [String] -> GmState -> GmState
setControl ctrl state = state { ctrl = ctrl }

eval :: GmState -> [GmState]
eval state = state : restStates
    where
        restStates
            | gmFinal state = []
            | otherwise     = eval nextState
        nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin state = state { stats = statIncSteps state.stats }

gmFinal :: GmState -> Bool
gmFinal state = null state.code

step :: GmState -> GmState
step state = case map toLower $ head state.ctrl of
    ""                -> state' { ctrl = drop 1 state.ctrl }
    "c"               -> state' { ctrl = repeat "" }
    '#': n
      | all isDigit n -> trace (iDisplay (inspect :: IseqRep)) (state' { ctrl = drop 1 state.ctrl })
            where
                inspect = iConcat [ iStr "         #"
                                  , iStr n, iStr " -> "
                                  , showNode state a node
                                  , iNewline, iNewline
                                  ]
                (a,_) = Stk.pop state.stack
                node  = hLookup state.heap a
    s | all isDigit s -> state' { ctrl = replicate (read s) "" ++ drop 1 state.ctrl }
      | otherwise     -> state' { ctrl = drop 1 state.ctrl }
    where
        state' = case state.code of
            i:is -> dispatch i (state { code = is, output = ""})
            []   -> error "already final state"

dispatch :: Instruction -> GmState -> GmState
dispatch instr = case instr of
    Unwind          -> unwind 
    PushGlobal f    -> pushGlobal f
    PushInt n       -> pushInt n
    Push n          -> push n
    Pop n           -> pop n
    Update n        -> update n
    MkAp            -> mkAp
    Slide n         -> slide n
    Alloc n         -> alloc n
    Eval            -> evalop
    Add             -> dyadicIntOp (+)
    Sub             -> dyadicIntOp (-)
    Mul             -> dyadicIntOp (*)
    Div             -> dyadicIntOp div
    Neg             -> unaryOp negate
    Eq              -> dyadicBoolOp (==)
    Ne              -> dyadicBoolOp (/=)
    Lt              -> dyadicBoolOp (<)
    Le              -> dyadicBoolOp (<=)
    Gt              -> dyadicBoolOp (>)
    Ge              -> dyadicBoolOp (>=)
    And             -> dyadicLogicOp (&&)
    Or              -> dyadicLogicOp (||)
    Not             -> unaryLogicOp not
    Cond ti fi      -> cond ti fi
    Pack tag arity  -> pack tag arity
    CaseJump alts   -> caseJump alts
    Split arity     -> split arity
    PushBasic n     -> pushBasic n
    MkBool          -> mkBool
    MkInt           -> mkInt
    UpdateInt n     -> updateInt n
    UpdateBool b    -> updateBool b
    Get             -> gmGet
    Return          -> gmReturn
    Print           -> gmPrint

unwind :: GmState -> GmState
unwind state
    = newState (hLookup state.heap a)
    where
        (a, stk) = Stk.pop state.stack
        newState node = case node of
            NNum _
                | isEmptyStack state.dump
                    -> state { ruleid = 10 }
                | otherwise
                    -> state { code  = i'
                             , stack = Stk.push a stk'
                             , vstack = vstk'
                             , dump  = dump'
                             , ruleid = 22
                             }
                where
                    ((i',stk',vstk'), dump') = Stk.pop state.dump
            NAp a1 _  -> state { code = [Unwind]
                               , stack = stk'
                               , ruleid = 11
                               }
                where
                    stk' = Stk.push a1 state.stack
            NInd a1   -> state { code = [Unwind]
                               , stack = Stk.push a1 stk
                               , ruleid = 17
                               }
            NGlobal n c
                | k < n
                    -> state { code  = i'
                             , stack = Stk.push ak stk'
                             , dump  = dump'
                             , vstack = vstk'
                             , ruleid = 29
                             }
                | otherwise
                    -> state { code = c
                             , stack = rearrange n state.heap state.stack
                             , ruleid = 19
                             }
                where 
                    k      = stk.curDepth
                    (ak,_) = Stk.pop $ Stk.discard k state.stack
                    ((i',stk',vstk'), dump') = Stk.pop state.dump
            NConstr _ _
                | isEmptyStack state.dump -> state
                | otherwise
                    -> state { code = i'
                            , stack = Stk.push a stk'
                            , vstack = vstk'
                            , dump  = dump'
                            , ruleid = 35
                            }
                where
                    ((i',stk',vstk'), dump') = Stk.pop state.dump 

pushGlobal :: GmGlobalMode -> GmState -> GmState
pushGlobal f state = case f of
    GlobalPack tag arity
        | show f `elem` aDomain state.globals
            -> state { stack = Stk.push a state.stack
                     , ruleid = 37
                     }
        | otherwise
            -> state { stack = Stk.push a state.stack
                     , heap = heap'
                     , globals = aInsert state.globals (show f) a
                     , ruleid = 38
                     }
            where
                node = NGlobal arity [Pack tag arity, Update 0, Unwind]
                (heap', a') = hAlloc state.heap node
                a = aLookup state.globals (show f) a'
                
    GlobalLabel name -> state { stack = Stk.push a state.stack
                              , ruleid = 5
                              }
        where
            a = aLookup state.globals name
                    (error $ "pushglobal: undeclared global: " ++ name)


pushInt :: Int -> GmState -> GmState
pushInt n state
    = case aLookup state.globals name (negate 1) of
        a' | a' < 0    -> state { stack = Stk.push a state.stack
                                , heap  = heap'
                                , globals = aInsert state.globals name a
                                , ruleid = 14
                                }
           | otherwise -> state { stack = Stk.push a' state.stack
                                , ruleid = 13
                                }
    where
        name = show n
        node = NNum n
        (heap',a) = hAlloc state.heap node

push :: Int -> GmState -> GmState
push n state
    = state { stack = Stk.push an state.stack
            , ruleid = 18 }
        where
            an = state.stack.stkItems !! n

pop :: Int -> GmState -> GmState
pop n state
    = state { stack = Stk.discard n state.stack
            , ruleid = 16
            }

update :: Int -> GmState -> GmState
update n state
    = state { stack  = stk'
            , heap   = heap'
            , ruleid = 15
            }
    where
        (a, stk') = Stk.pop state.stack
        heap' = hUpdate state.heap (stk'.stkItems !! n) (NInd a)

mkAp :: GmState -> GmState
mkAp state
    = state { stack = stk'
            , heap  = heap'
            , ruleid = 7 }
    where
        (a1, stk1) = Stk.pop state.stack
        (a2, stk2) = Stk.pop stk1
        (heap', a) = hAlloc state.heap (NAp a1 a2)
        stk'       = Stk.push a stk2

slide :: Int -> GmState -> GmState
slide n state
    = state { stack = stk'
            , ruleid = 5 }
    where
        (a, stk) = Stk.pop state.stack
        stk' = Stk.push a (Stk.discard n stk)

alloc :: Int -> GmState -> GmState
alloc n state
    = state 
    { stack = foldr Stk.push state.stack as
    , heap  = heap'
    , ruleid = 20
    }
    where
        (heap', as) = allocNodes n state.heap

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes m heap = case m of
    0   -> (heap, [])
    n+1 -> (heap2, a:as)
        where
            (heap1, as) = allocNodes n heap
            (heap2, a ) = hAlloc heap1 (NInd hNull)
    _   -> error "allocNodes: negative number"

evalop :: GmState -> GmState
evalop state
    = state
    { code   = [Unwind]
    , stack  = Stk.push a emptyStack
    , vstack = emptyStack
    , dump   = Stk.push (state.code, stk, state.vstack) state.dump
    , ruleid = 48
    }
    where
        (a,stk) = Stk.pop state.stack

dyadicIntOp :: (Int -> Int -> Int) -> GmState -> GmState
dyadicIntOp bop state
    = state { vstack = Stk.push (a1 `bop` a2) vstk2
            , ruleid = 39 
            }
    where
        (a1, vstk1) = Stk.pop state.vstack
        (a2, vstk2) = Stk.pop vstk1

dyadicBoolOp :: (Int -> Int -> Bool) -> GmState -> GmState
dyadicBoolOp = dyadicIntOp . ((conv .) .)
    where
        conv False = 1
        conv True  = 2

dyadicLogicOp :: (Bool -> Bool -> Bool) -> GmState -> GmState
dyadicLogicOp op = dyadicBoolOp (op `on` conv)
    where
        conv 1 = False
        conv 2 = True
        conv _ = error "not Boolean"

unaryLogicOp :: (Bool -> Bool) -> GmState -> GmState
unaryLogicOp op = unaryOp (enc . op . dec)
    where
        dec 1 = False
        dec 2 = True
        dec _ = error "not Boolean"
        enc False = 1
        enc True  = 2

unaryOp :: (Int -> Int) -> GmState -> GmState
unaryOp uop state 
    = state 
    { vstack = Stk.push (uop v) vstk'
    , ruleid = 40
    }
    where
        (v, vstk') = Stk.pop state.vstack

cond :: GmCode -> GmCode -> GmState -> GmState
cond ti fi state
    = state { code   = cont ++ state.code
            , vstack = vstk'
            , ruleid = rid
            }
    where
        (v,vstk') = Stk.pop state.vstack 
        (cont,rid) = case v of
            2 -> (ti,46)
            1 -> (fi,47)
            _ -> error "not boolean"

pack :: Tag -> Arity -> GmState -> GmState
pack tag arity state
    = state { stack = Stk.push a stk'
            , heap  = heap'
            , ruleid = 30
            }
    where
        (as, stk') = Stk.npop arity state.stack
        (heap', a) = hAlloc state.heap (NConstr tag as)

caseJump :: [(Int, GmCode)] -> GmState -> GmState
caseJump alts state
    = state { code   = cont ++ state.code
            , ruleid = 31
            }
    where
        (a, _) = Stk.pop state.stack
        cont = case hLookup state.heap a of
            NConstr t _
                -> aLookup alts t
                    (error $ "No case for constructor<" ++ show t ++ ">")
            n   -> error $ "casejump: Not data structure: " ++ show n

split :: Int -> GmState -> GmState
split n state
    = state { stack = stk' 
            , ruleid = 32
            }
    where
        (a, stk) = Stk.pop state.stack
        stk' = case hLookup state.heap a of
            NConstr _ as
                | n == length as -> foldr Stk.push stk as
                | otherwise      -> error "Not saturated"
            _                    -> error "Not data structure"

pushBasic :: Int -> GmState -> GmState
pushBasic n state
    = state
    { vstack = Stk.push n state.vstack
    , ruleid = 41
    }

mkBool :: GmState -> GmState
mkBool state
    = state
    { stack = Stk.push addr state.stack 
    , vstack = vstk'
    , heap = heap'
    , ruleid = 42
    }
    where
        (tag, vstk') = Stk.pop state.vstack
        (heap',addr) = hAlloc state.heap (NConstr tag [])

mkInt  :: GmState -> GmState
mkInt state
    = state
    { stack = Stk.push addr state.stack
    , vstack = vstk'
    , heap = heap'
    , ruleid = 43
    }
    where
        (n,vstk') = Stk.pop state.vstack
        (heap',addr) = hAlloc state.heap (NNum n)

updateBool :: Int -> GmState -> GmState
updateBool n state
    = state'
    { stack = stk'
    , heap  = heap'
    }
    where
        state' = mkBool state
        (a, stk') = Stk.pop state'.stack
        node = hLookup state'.heap a
        a' = stk'.stkItems !! n
        heap' = hUpdate state'.heap a' node

updateInt :: Int -> GmState -> GmState
updateInt n state
    = state'
    { stack = stk'
    , heap  = heap'
    }
    where
        state' = mkInt state
        (a, stk') = Stk.pop state'.stack
        node = hLookup state'.heap a
        a' = stk'.stkItems !! n
        heap' = hUpdate state'.heap a' node

gmGet :: GmState -> GmState
gmGet state
    = state
    { stack = stk'
    , vstack = Stk.push n state.vstack
    , ruleid = rid
    }
    where
        (a,stk') = Stk.pop state.stack
        (n,rid) = case hLookup state.heap a of
            NConstr t [] -> (t,44)
            NNum m       -> (m,45)
            _            -> error "invalid node for Get"

gmReturn :: GmState -> GmState
gmReturn state
    = state
    { code = cont
    , stack = stk'
    , vstack = vstk
    , dump = dmp
    }
    where
        ((cont,stk,vstk),dmp) = Stk.pop state.dump
        ak = last state.stack.stkItems
        stk' = Stk.push ak stk

gmPrint :: GmState -> GmState
gmPrint state
    = case hLookup state.heap a of
        NNum n
            -> state { output = show n
                     , stack  = stk
                     , ruleid = 33
                     }
        NConstr t as 
            -> state { output = showConstr t (length as)
                     , code   = printCode (length as) ++ state.code
                     , stack  = foldr Stk.push stk as
                     , ruleid = 34
                     }
        _   -> error "gmprint: cannot print"
    where
        (a, stk) = Stk.pop state.stack
        showConstr tag arity
            = "Pack{" ++ show tag ++ "," ++ show arity ++ "}"
        printCode n = concat $ take n $ cycle [[Eval, Print]]

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stk
    = foldr phi (Stk.discard n stk) $ take n $ drop 1 stk.stkItems
    where
        phi a = Stk.push (getArg (hLookup heap a))
getArg :: Node -> Addr
getArg (NAp _ a2) = a2
getArg _          = error "getArg: Not application node"

{-
boxInteger :: Int -> GmState -> GmState
boxInteger n state
    = state { stack = Stk.push addr state.stack
            , heap  = heap'
            }
    where
        (heap', addr) = hAlloc state.heap (NNum n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
    = ub (hLookup state.heap a)
    where
        ub node = case node of
            NNum i -> i
            _      -> error "unboxInteger: Unboxing a non-integer"

primitive1 :: (b -> GmState -> GmState) -- ^ boxing function
           -> (Addr -> GmState -> a)    -- ^ unboxing function
           -> (a -> b)                  -- ^ operator
           -> (GmState -> GmState)      -- ^ state transition
primitive1 box unbox op state
    = box (op (unbox a state)) (state { stack = stack', ruleid = 25})
    where
        (a, stack') = Stk.pop state.stack

primitive2 :: (b -> GmState -> GmState) -- ^ boxing function
           -> (Addr -> GmState -> a)    -- ^ unboxing function
           -> (a -> a -> b)             -- ^ operator
           -> (GmState -> GmState)      -- ^ state transition
primitive2 box unbox op state
    = box (op (unbox a0 state) (unbox a1 state)) (state { stack = stack'', ruleid = 24})
    where
        (a0, stack')  = Stk.pop state.stack
        (a1, stack'') = Stk.pop stack'

boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
    = state { stack = Stk.push addr state.stack 
            , heap  = heap' }
    where
        (heap', addr) = hAlloc state.heap (NConstr tag [])
        tag | b         = 2
            | otherwise = 1
-}
