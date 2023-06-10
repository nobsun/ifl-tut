{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gmachine.Mark7.Machine
    where

import Data.Char
import Data.List
-- import Text.ParserCombinators.ReadP

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, discard)
import Utils
import Iseq

import Gmachine.Mark7.Code ( GmCode, Instruction(..) )
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
            i:is -> dispatch i (state { code = is, output = ""})
            []   -> error "already final state"

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f) = pushglobal f
dispatch (PushInt n)    = pushint n
dispatch (PushBasic n)  = pushbasic n
dispatch MkInt          = mkint
dispatch MkBool         = mkbool
dispatch MkAp           = mkap
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
dispatch (CaseJump alts) = casejump alts
dispatch (Split n)       = split n
dispatch Print           = gmprint
dispatch Get             = gmget

pushglobal :: Name -> GmState -> GmState
pushglobal f state = case readsPack f of
    EConstr tag arity : _
        | f `elem` aDomain state.globals
            -> state { stack = Stk.push a state.stack
                     , ruleid = 37
                     }
        | otherwise
            -> state { stack = Stk.push a state.stack
                     , heap = heap'
                     , globals = aInsert state.globals (showconstr tag arity) a
                     , ruleid = 38
                     }
            where
                node = NGlobal arity [Pack tag arity, Update 0, Unwind]
                (heap', a') = hAlloc state.heap node
                a = aLookup state.globals f a'
    _   -> state { stack = Stk.push a state.stack
                 , ruleid = 5
                 }
        where
            a = aLookup state.globals f (error $ "pushglobal: undeclared global: " ++ f)

readsPack :: Name -> [CoreExpr]
readsPack = map fst . filter (null . snd) . pEConstr . clex 0


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

pushbasic :: Int -> GmState -> GmState
pushbasic n state
    = state
    { vstack = Stk.push n state.vstack
    , ruleid = 41
    }

mkint :: GmState -> GmState
mkint state
    = state
    { stack  = Stk.push a state.stack
    , vstack = vstack'
    , heap   = heap'
    , ruleid = 43
    }
    where
        (n, vstack') = Stk.pop state.vstack
        (heap', a)   = hAlloc state.heap (NNum n)

mkbool :: GmState -> GmState
mkbool state
    = state
    { stack  = Stk.push a state.stack
    , vstack = vstack'
    , heap   = heap'
    , ruleid = 42
    }
    where
        (t, vstack') = Stk.pop state.vstack
        (heap', a)   = hAlloc state.heap (NConstr t [])

gmget :: GmState -> GmState
gmget state
    = state
    { stack  = stack'
    , vstack = Stk.push m state.vstack
    , ruleid = rid
    }
    where
        (a, stack') = Stk.pop state.stack
        (m, rid) = case hLookup state.heap a of
            NNum n       -> (n, 45)
            NConstr t [] -> (t, 44)
            _            -> error ("gmget: invalid node")

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
            an = traceShow (length state.stack.stkItems) state.stack.stkItems !! n

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
arithmetic1 op state
    = state
    { vstack = case Stk.pop state.vstack of
        (n, vs) -> Stk.push (op n) vs
    , ruleid = 40
    }


arithmetic2 :: (Int -> Int -> Int) -> GmState -> GmState
arithmetic2 op state
    = state
    { vstack = case Stk.npop 2 state.vstack of
        ([n0, n1], vs) -> Stk.push (n0 `op` n1) vs
        _              -> error "arithmetic2: too short vstack"
    , ruleid = 39
    }

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison cmp state
    = state
    { vstack = case Stk.npop 2 state.vstack of
        ([n0, n1], vs) -> Stk.push (fromEnum (n0 `cmp` n1)) vs
        _              -> error "comparison: too short vstack"        
    }

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
cond i1 i2 state
    | v == fromEnum True  = state { code = i1 ++ state.code
                                   , vstack = vstack'
                                   , ruleid = 46
                                   }
    | v == fromEnum False = state { code = i2 ++ state.code
                                   , vstack = vstack'
                                   , ruleid = 47
                                   }
    | otherwise           = error "cond: invalid node"
    where
        (v, vstack') = Stk.pop state.vstack

pack :: Tag -> Arity -> GmState -> GmState
pack t n state
    = state { stack = Stk.push a stack'
            , heap  = heap'
            , ruleid = 30
            }
    where
        (as, stack') = Stk.npop n state.stack
        (heap', a)   = hAlloc state.heap (NConstr t as)

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
            n   -> error $ "casejump: Not data structure: "
                        ++ show n

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
            -> state { output = showconstr t (length as)
                     , code   = printcode (length as) ++ state.code
                     , stack  = foldr Stk.push stk as
                     , ruleid = 34
                     }
        _   -> error "gmprint: cannot print"
    where
        (a, stk) = Stk.pop state.stack

showconstr :: Tag -> Arity -> String
showconstr tag arity
    = "Pack{" ++ show tag ++ "," ++ show arity ++ "}"

printcode :: Int -> GmCode
printcode n = concat $ take n $ cycle [[Eval, Print]]

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
    , vstack  = emptyStack
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
        compiled 
            = map compileSc 
                (preludeDefs ++ extraPreludeDefs ++ program)
            ++ compiledPrimitives

extraPreludeDefs :: [CoreScDefn]
extraPreludeDefs = parse extraPrelude

extraPrelude :: String
extraPrelude = unlines
    [ "if c t f = case c of"
    , "             <0> -> f ;"
    , "             <1> -> t"
    ]

primitives :: [CoreScDefn]
primitives
    = [ ("False", [], EConstr (fromEnum False) 0)
      , ("True" , [], EConstr (fromEnum True)  0)
      ]

type GmCompiledSC = (Name, Arity, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, arity, instrs)
    = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal arity instrs)

initialCode :: GmCode
initialCode = [PushGlobal "main", Eval, Print]

oldInitialCode :: GmCode
oldInitialCode = [PushGlobal "main", Unwind]

compileSc :: CoreScDefn -> GmCompiledSC
compileSc (name, args, body)
    = (name, length args, compileR body (zip args [0 ..]))

compileR :: GmCompiler
compileR e args = compileE e args ++ [Update n, Pop n, Unwind]
    where
        n = length args

compileE :: GmCompiler
compileE expr env = case expr of
    ENum n -> [PushInt n]
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
    ECase e alts    -> compileE e env ++ [CaseJump (compileAlts compileE' alts env)]
    _ -> compileC expr env ++ [Eval]

builtInDyadic :: Assoc Name Instruction
builtInDyadic 
    = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
      , ("==", Eq), ("/=", Ne), (">", Gt), (">=", Ge), ("<", Lt), ("<=", Le)]

compileC :: GmCompiler
compileC expr env = case expr of
    EVar v
        | v `elem` aDomain env -> [Push a]
        | otherwise            -> [PushGlobal v]
        where
            a = aLookup env v (error "compileC: Cannot happen")
    ENum n  -> [PushInt n]
    EAp e1 e2 -> case spines expr of
        [] -> compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
        ss -> compileCS ss env
        where
            spines = iter 0 []
            iter a ss e = case e of
                EAp e1' e2'        -> iter (succ a) (e2':ss) e1'
                EConstr _ arity  
                    | a == arity -> reverse (e : ss)
                _                -> []
    ELet recflg defs e
        | recflg    -> compileLetrec compileC defs e env
        | otherwise -> compileLet compileC defs e env
    ECase e _as
            -> compileE e env -- ++ [Casejump (compileAlts as env)]
    EConstr tag 0
            -> [Pack tag 0]
    EConstr tag a
            -> [PushGlobal (showconstr tag a)]
    _       -> error $ "compileC: Not implemented for: " ++ show expr
                               
compileCS :: [CoreExpr] -> GmEnvironment -> [Instruction]
compileCS exprs env = case exprs of
    [EConstr tag arity] -> [Pack tag arity]
    e : es              -> compileC e env ++ compileCS es (argOffset 1 env)
    []                  -> error "compileCS: empty exprs"


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
