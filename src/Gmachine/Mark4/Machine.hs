{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gmachine.Mark4.Machine
    where

import Data.List

import Language
import Heap
import qualified Stack as Stk (push, pop, discard)
import Stack hiding (push, pop, discard)
import Utils

import Gmachine.Mark4.Code ( GmCode, Instruction(..) )
import Gmachine.Mark4.Node
import Gmachine.Mark4.PPrint
import Gmachine.Mark4.State

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
run prog inputs = showResults 
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
step state = case state.code of
    i:is -> dispatch i (state { code = is })
    []   -> error "already final state"

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Slide n)      = slide n
dispatch (Push n)       = push n
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch Unwind         = unwind
dispatch (Alloc n)      = alloc n
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 div
dispatch Neg            = arithmetic1 negate
dispatch Eq             = comparison (==)
dispatch Ne             = comparison (/=)
dispatch Lt             = comparison (<)
dispatch Le             = comparison (<=)
dispatch Gt             = comparison (>)
dispatch Ge             = comparison (>=)
dispatch Eval           = evalop
dispatch (Cond i1 i2)   = cond i1 i2

pushglobal :: Name -> GmState -> GmState
pushglobal f state
    = state { stack = Stk.push a state.stack
            , ruleid = 5
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
            , ruleid = 7 }
    where
        (a1, stk')  = Stk.pop state.stack
        (a2, stk'') = Stk.pop stk'
        (heap', a)  = hAlloc state.heap (NAp a1 a2)
        stack'      = Stk.push a stk''

push :: Int -> GmState -> GmState
push n state
    = state { stack = Stk.push an state.stack
            , ruleid = 18 }
        where
            an = state.stack.stkItems !! n

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

slide :: Int -> GmState -> GmState
slide n state
    = state { stack = stack'
            , ruleid = 5 }
    where
        (a, stk) = Stk.pop state.stack
        stack' = Stk.push a (Stk.discard n stk)

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
                             , dump  = dump'
                             , ruleid = 22
                             }
                where
                    ((i',stk'), dump') = Stk.pop state.dump
            NAp a1 _  -> state { code = [Unwind]
                               , stack = stack'
                               , ruleid = 11 }
                where
                    stack' = Stk.push a1 state.stack
            NInd a1   -> state { code = [Unwind]
                               , stack = Stk.push a1 stk
                               , ruleid = 17
                               }
            NGlobal n c
                | stk.curDepth < n -> error "Unwinding with too few artuments"
                | otherwise 
                    -> state { code = c
                             , stack = rearrange n state.heap stk
                             , ruleid = 19
                             }
                        where
                            phi a = case hLookup state.heap a of
                                NAp _ a' -> Stk.push a' 

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stk
    = foldr phi (Stk.discard n stk) $ take n stk.stkItems
    where
        phi a = Stk.push (getArg (hLookup heap a))

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
allocNodes 0     heap = (heap, [])
allocNodes (n+1) heap = (heap2, a:as)
    where
        (heap1, as) = allocNodes n heap
        (heap2, a ) = hAlloc heap1 (NInd hNull)

arithmetic1 :: (Int -> Int) -> GmState -> GmState
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GmState -> GmState
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

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
        (heap', addr) = hAlloc state.heap (NNum b')
        b' | b         = 1
           | otherwise = 0

evalop :: GmState -> GmState
evalop state
    = state
    { code = [Unwind]
    , stack = Stk.push a emptyStack
    , dump  = dump'
    , ruleid = 23
    }
    where
        (a, stack') = Stk.pop state.stack
        dump' = Stk.push (state.code, stack') state.dump

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = case hLookup state.heap a of
    NNum 1 -> state { code = i1 ++ state.code
                    , stack = stack'
                    , ruleid = 21
                    }
    NNum 0 -> state { code = i2 ++ state.code
                    , stack = stack'
                    , ruleid = 22
                    }
    where
        (a, stack') = Stk.pop state.stack

--

defaultHeapSize :: Int
defaultHeapSize = 1024 ^ (2 :: Int)

defaultThreshold :: Int
defaultThreshold = 50

compile :: CoreProgram -> GmState
compile program
    = GmState
    { ctrl  = []
    , code  = initialCode
    , stack = emptyStack
    , dump  = emptyStack
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

compileC :: GmCompiler
compileC expr env = case expr of
    EVar v
        | v `elem` aDomain env -> [Push a]
        | otherwise            -> [Pushglobal v]
        where
            a = aLookup env v (error "compileC: Cannot happen")
    ENum n  -> [Pushint n]
    EAp e1 e2
            -> compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
    ELet recursive defs e
        | recursive -> compileLetrec compileC defs e env
        | otherwise -> compileLet compileC defs e env
    _       -> error "Not implemented"

compileLet :: GmCompiler -> Assoc Name CoreExpr -> GmCompiler
compileLet comp defs expr env
    = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where
        env' = compileArgs defs env

compileLet' :: Assoc Name CoreExpr -> GmEnvironment -> GmCode
compileLet' [] env = []
compileLet' ((name, expr):defs) env
    = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileArgs :: Assoc Name CoreExpr -> GmEnvironment -> GmEnvironment
compileArgs defs env
    = zip (aDomain defs) [n-1, n-2 .. 0] ++ argOffset n env
    where
        n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, m+n) | (v, m) <- env ]

compileLetrec :: GmCompiler -> Assoc Name CoreExpr -> GmCompiler
compileLetrec comp defs e env
    =  [Alloc n]
    ++ compiled defs (n-1)
    ++ comp e newArgs
    ++ [Slide n]
    where
        newArgs = compileArgs defs env
        n = length defs
        compiled dds i = case dds of
            []   -> []
            d:ds -> compileC (snd d) newArgs
                 ++ [Update i]
                 ++ compiled ds (i-1)

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []
