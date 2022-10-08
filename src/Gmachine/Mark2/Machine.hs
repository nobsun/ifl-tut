{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gmachine.Mark2.Machine
    where

import Data.List

import Language
import Heap
import qualified Stack as Stk (push, pop, discard)
import Stack hiding (push, pop, discard)
import Utils

import Gmachine.Mark2.Code
import Gmachine.Mark2.Node
import Gmachine.Mark2.PPrint
import Gmachine.Mark2.State

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

run :: String -> String
run = showResults . eval . compile . parse

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
step state = case state.code of
    i:is -> dispatch i (state { code = is })
    []   -> error "already final state"

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f state
    = state { stack = Stk.push a state.stack
            , ruleid = 1
            }
    where
        a = aLookup state.globals f (error $ "Undeclared global " ++f)

pushint :: Int -> GmState -> GmState
pushint n state
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
        (heap', a) = hAlloc state.heap node

mkap :: GmState -> GmState
mkap state
    = state { stack = stack'
            , heap  = heap'
            , ruleid = 3 }
    where
        (a1, stk')  = Stk.pop state.stack
        (a2, stk'') = Stk.pop stk'
        (heap', a)  = hAlloc state.heap (NAp a1 a2)
        stack'      = Stk.push a stk''

push :: Int -> GmState -> GmState
push n state
    = state { stack = Stk.push a state.stack
            , ruleid = 4 }
    where
        a = getArg (hLookup state.heap (state.stack.stkItems !! (n+1)))

getArg :: Node -> Addr
getArg (NAp _ a2) = a2
getArg _          = error "getArg: Not application node"

update :: Int -> GmState -> GmState
update n state
    = state { stack  = stack'
            , heap   = heap'
            , ruleid = 15
            }
    where
        (a, stack') = Stk.pop state.stack
        heap' = hUpdate state.heap (stack'.stkItems !! n) (NInd a)

pop :: Int -> GmState -> GmState
pop n state
    = state { stack = Stk.discard n state.stack
            , ruleid = 16
            }

unwind :: GmState -> GmState
unwind state
    = newState (hLookup state.heap a)
    where
        (a, stk) = Stk.pop state.stack
        newState node = case node of
            NNum _    -> state { ruleid = 6 }
            NAp a1 _  -> state { code = [Unwind]
                               , stack = stack'
                               , ruleid = 7 }
                where
                    stack' = Stk.push a1 state.stack
            NGlobal n c
                | stk.curDepth < n -> error "Unwinding with too few artuments"
                | otherwise        -> state { code = c
                                            , ruleid = 8 }
            NInd a1   -> state { code = [Unwind]
                               , stack = Stk.push a1 stk
                               , ruleid = 17
                               }

defaultHeapSize :: Int
defaultHeapSize = 1024 ^ (2 :: Int)

defaultThreshold :: Int
defaultThreshold = 50

compile :: CoreProgram -> GmState
compile program
    = GmState
    { code  = initialCode
    , stack = emptyStack
    , heap  = heap'
    , globals = globals'
    , stats = statInitial
    , ruleid = 0
    }
    where
        (heap', globals') = let { ?sz = defaultHeapSize; ?th = defaultThreshold }
                            in buildInitialHeap program

buildInitialHeap :: (?sz :: Int, ?th :: Int) => CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
    = mapAccumL allocateSc hInitial compiled
    where
        compiled :: [GmCompiledSC]
        compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives
        -- compiled = map compileSc program

type GmCompiledSC = (Name, Arity, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, arity, instrs)
    = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal arity instrs)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

{- | スーパーコンビネータのコンパイル
>>> compileSc ("K", ["x", "y"], EVar "x")
("K",2,[Push 0,Slide 3,Unwind])
-}
compileSc :: CoreScDefn -> GmCompiledSC
compileSc (name, args, body)
    = (name, length args, compileR body (zip args [0 ..]))

compileR :: GmCompiler
compileR e args = compileC e args ++ [Update n, Pop n, Unwind]
    where
        n = length args

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = Assoc Name Addr

compileC :: GmCompiler
compileC expr env = case expr of
    EVar v
        | elem v (aDomain env) -> [Push a]
        | otherwise            -> [Pushglobal v]
        where
            a = aLookup env v (error "compileC: Cannot happen")
    ENum n  -> [Pushint n]
    EAp e1 e2
            -> compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
    _       -> error "Not implemented"

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, m+n) | (v, m) <- env ]

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []

