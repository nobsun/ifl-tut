{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gmachine.Mark6.Machine
    where

import Data.Char
import Data.List

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, discard)
import Utils
import Iseq

import Gmachine.Mark6.Code ( GmCode, Instruction(..) )
import Gmachine.Mark6.Node
import Gmachine.Mark6.PPrint
import Gmachine.Mark6.State

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
    $ parse (extraPrelude ++ prog)

extraPrelude :: String
extraPrelude = unlines
    [ "False = Pack{1,0} ;"
    , "True  = Pack{2,0} ;"
    , "if c t f = case c of"
    , "                <1> -> f ;"
    , "                <2> -> t ;"
    , "nil = Pack{1,0} ;"
    , "cons x xs = Pack{2,2} x xs ;"
    ]

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
    ""                -> state' { ctrl = tail state.ctrl }
    "c"               -> state' { ctrl = repeat "" }
    '#': n
      | all isDigit n -> trace (iDisplay (inspect :: IseqRep)) (state' { ctrl = tail state.ctrl })
            where
                inspect = iConcat [ iStr "         #"
                                  , iStr n, iStr " -> "
                                  , showNode state a node
                                  , iNewline, iNewline
                                  ]
                (a,_) = Stk.pop state.stack
                node  = hLookup state.heap a
    s | all isDigit s -> state' { ctrl = replicate (read s) "" ++ tail state.ctrl }
      | otherwise     -> state' { ctrl = tail state.ctrl }
    where
        state' = case state.code of
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
dispatch Unwind          = unwind
dispatch (Alloc n)       = alloc n
dispatch Add             = arithmetic2 (+)
dispatch Sub             = arithmetic2 (-)
dispatch Mul             = arithmetic2 (*)
dispatch Div             = arithmetic2 div
dispatch Neg             = arithmetic1 negate
dispatch Eq              = comparison (==)
dispatch Ne              = comparison (/=)
dispatch Lt              = comparison (<)
dispatch Le              = comparison (<=)
dispatch Gt              = comparison (>)
dispatch Ge              = comparison (>=)
dispatch Eval            = evalop
dispatch (Cond i1 i2)    = cond i1 i2
dispatch (Pack t a)      = pack t a
dispatch (Casejump alts) = casejump alts
dispatch (Split n)       = split n
dispatch Print           = gmprint

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
                | k < n
                    -> state { code  = i'
                             , stack = Stk.push ak stk'
                             , dump  = dump'
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
                    ((i',stk'), dump') = Stk.pop state.dump
            NConstr _t _as
                -> state { code = i'
                         , stack = Stk.push a' stk'
                         , dump  = dump'
                         , ruleid = 35
                         }
                where
                    (a',_)             = Stk.pop state.stack
                    ((i',stk'), dump') = Stk.pop state.dump 

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stk
    = foldr phi (Stk.discard n stk) $ take n $ tail stk.stkItems
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
allocNodes _ _ = error "allocNodes: negative number"

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
    _ -> error "cond: invalid node"
    where
        (a, stack') = Stk.pop state.stack

pack :: Tag -> Arity -> GmState -> GmState
pack t n state
    = state { stack = stack'
            , heap  = heap'
            , ruleid = 30
            }
    where
        (as, stack') = Stk.npop n state.stack
        (heap', _a)   = hAlloc state.heap (NConstr t as)

casejump :: [(Int, GmCode)] -> GmState -> GmState
casejump alts state
    = state { code   = i ++ state.code
            , ruleid = 31
            }
    where
        (a, _) = Stk.pop state.stack
        i = case hLookup state.heap a of
            NConstr t _ss
                -> aLookup alts t (error $ "No case for constructor" ++ show t)
            _   -> error "Not data structure"

split :: Int -> GmState -> GmState
split n state
    = state { stack = stack' 
            , ruleid = 32
            }
    where
        (a, stk) = Stk.pop state.stack
        stack' = case hLookup state.heap a of
            NConstr _t as
                | n == length as -> foldr Stk.push stk as
                | otherwise      -> error "Not saturated"
            _                    -> error "Not data structure"

gmprint :: GmState -> GmState
gmprint state
    = case hLookup state.heap a of
        NNum n
            -> state { output = show n
                     , stack  = stk
                     , ruleid = 33
                     }
        NConstr t as 
            -> state { output = showconstr t as
                     , code   = printcode (length as) ++ state.code
                     , stack  = foldr Stk.push stk as
                     , ruleid = 34
                     }
        _   -> error "gmprint: cannot print"
    where
        (a, stk) = Stk.pop state.stack
        showconstr tag ary = "Pack{"
                           ++ show tag
                           ++ ","
                           ++ show (length ary)
                           ++ "}"

printcode :: Int -> GmCode
printcode n = take n $ cycle [Eval, Print]

--

defaultHeapSize :: Int
defaultHeapSize = 1024 ^ (2 :: Int)

defaultThreshold :: Int
defaultThreshold = 50

--

compile :: CoreProgram -> GmState
compile program
    = GmState
    { ctrl    = []
    , output  = ""
    , code    = initialCode
    , stack   = emptyStack
    , dump    = emptyStack
    , heap    = heap'
    , globals = globals'
    , stats   = statInitial
    , ruleid  = 0
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

type GmCompiledSC = (Name, Arity, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, arity, instrs)
    = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal arity instrs)

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval, Print]

oldInitialCode :: GmCode
oldInitialCode = [Pushglobal "main", Unwind]

compileSc :: CoreScDefn -> GmCompiledSC
compileSc (name, args, body)
    = (name, length args, compileR body (zip args [0 ..]))

compileR :: GmCompiler
compileR e args = compileE e args ++ [Update n, Pop n, Unwind]
    where
        n = length args

compileE :: GmCompiler
compileE expr env = case expr of
    ENum n -> [Pushint n]
    ELet isRec defs e
        | isRec     -> compileLetrec compileE defs e env
        | otherwise -> compileLet    compileE defs e env
    EAp (EAp (EVar name) e0) e1
        | name `elem` aDomain builtInDyadic
                    -> compileE e1 env ++ compileE e0 (argOffset 1 env) ++ [dyadic]
            where
                dyadic = aLookup builtInDyadic name (error "Invalid dyadic operator" )
    EAp (EVar "negate") e
                    -> compileE e env ++ [Neg]
    EAp (EAp (EAp (EVar "if") e0) e1) e2
                    -> compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
    ECase e alts    -> compileE e env ++ [Casejump (compileAlts compileE' alts env)]
    _ -> compileC expr env ++ [Eval]

builtInDyadic :: Assoc Name Instruction
builtInDyadic 
    = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
      , ("==", Eq), ("/=", Ne), (">", Gt), (">=", Ge), ("<", Lt), ("<=", Le)]

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
    ELet recflg defs e
        | recflg    -> compileLetrec compileC defs e env
        | otherwise -> compileLet compileC defs e env
    ECase e _as
            -> compileE e env -- ++ [Casejump (compileAlts as env)]
    _       -> error $ "Not implemented" ++ show expr

compileLet :: GmCompiler -> Assoc Name CoreExpr -> GmCompiler
compileLet comp defs expr env
    = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where
        env' = compileArgs defs env

compileLet' :: Assoc Name CoreExpr -> GmEnvironment -> GmCode
compileLet' [] _env = []
compileLet' ((_name, expr):defs) env
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
compiledPrimitives
    = [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
      , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
      , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
      , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
      , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
      , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
      , ("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
      , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
      , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
      , (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
      , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
    --   , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
      ]

compileAlts :: (Int -> GmCompiler)
            -> [CoreAlt]
            -> GmEnvironment
            -> [(Int, GmCode)]
compileAlts comp alts env
    = [ (tag, comp len body (zip names [0 ..] ++ argOffset len env))
      | (tag, names, body) <- alts, let len = length names ]

compileE' :: Int -> GmCompiler
compileE' offset expr env
    = [Split offset] ++ compileE expr env ++ [Slide offset]

--

sample :: String
sample = unlines
    [ "length xs = case xs of"
    , "                   <1> -> 0 ;"
    , "                   <2> y ys -> 1 + length ys"
    ]
