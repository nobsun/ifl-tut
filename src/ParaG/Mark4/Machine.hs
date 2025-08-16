{-# LANGUAGE CPP #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ParaG.Mark4.Machine
    where

import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import ParaG.Mark4.Code
import ParaG.Mark4.Compiler
import ParaG.Mark4.PPrint
import ParaG.Mark4.State

import Debug.Trace qualified as Deb

#ifdef __TRACE__
trace :: String -> a -> a
trace = Deb.trace
tracing :: Show a => a -> a
tracing = trace . show <*> id
#else
trace :: String -> a -> a
trace = const id
tracing :: Show a => a -> a
tracing = id
#endif

machineSize :: Int
machineSize = 4

run :: String -> ([String] -> [String])
run prog inputs 
    = showFullResults 
    $ eval 
    $ setControl inputs
    $ compile 
    $ parse prog

setControl :: [String] -> PgmState -> PgmState
setControl ctrl state = state { ctrl = ctrl }

eval :: PgmState -> [PgmState]
eval state = state : restStates
    where
        restStates
            | gmFinal state = []
            | otherwise     = eval nextState
        nextState = doAdmin (steps state)

doAdmin :: PgmState -> PgmState
doAdmin state = state { pgmGlobal = global
                      , pgmLocals = catMaybes locals
                      }
    where
        (global, locals) = mapAccumL phi state.pgmGlobal state.pgmLocals
        phi g = \ case
            l | null l.code -> (g'', Nothing)   -- finished local task, so unlocked 
              | otherwise   -> (g , Just l)
              where
                (g',l') = foldl (flip unlock) (g,l) l.locks
                g'' = g' { stats = g'.stats { durations = l'.clock : g'.stats.durations}}
        
-- doAdmin :: PgmState -> PgmState
-- doAdmin state = state { pgmGlobal = state.pgmGlobal { stats = stats'}
--                       , pgmLocals = locals 
--                       }
--     where
--         (locals, stats') = foldr phi ([], state.pgmGlobal.stats) state.pgmLocals
--         phi l (ls, sts) 
--             | null l.code = (ls, sts { durations = l.clock : sts.durations })
--             | otherwise   = (l:ls, sts)

gmFinal :: PgmState -> Bool
gmFinal state = null state.pgmLocals && null state.pgmGlobal.sparks

steps :: PgmState -> PgmState
steps state = case concatMap (map toLower) $ take 1 state.ctrl of
    ""                -> state' { ctrl = drop 1 state.ctrl }
    "c"               -> state' { ctrl = repeat "" }
    s | all isDigit s -> state' { ctrl = replicate (read s) "" ++ drop 1 state.ctrl }
      | otherwise     -> state' { ctrl = drop 1 state.ctrl }
    where
        state' = scheduler 
               $ state { pgmGlobal = global'
                       , pgmLocals = locals'
                       }
        locals' = state.pgmLocals ++ newtasks
        (global', newtasks) = (state.pgmGlobal { sparks = rsp }, sps)
        (sps, rsp) = splitAt (machineSize - length state.pgmLocals) state.pgmGlobal.sparks

scheduler :: PgmState -> PgmState
scheduler state 
    = state { pgmGlobal = global
            , pgmLocals = nonRunning ++ running
            }
    where
        (global, running) = mapAccumL step 
                            state.pgmGlobal 
                            (map tick (take machineSize state.pgmLocals))
        nonRunning = drop machineSize state.pgmLocals

makeTask :: GmTaskId -> Addr -> PgmLocalState
makeTask tid addr = PgmLocalState
    { code   = [Eval]
    , stack  = singletonStack addr
    , dump   = emptyStack 
    , vstack = emptyStack
    , locks  = []
    , clock  = 0
    , taskid = tid
    , ruleid = 0
    }

tick :: PgmLocalState -> PgmLocalState
tick local = local { clock = succ local.clock }

lock :: Addr -> GmState -> GmState
lock addr (global, local)
    = ( global { heap = heap' }
      , local  { locks = addr : local.locks }
      )
    where
        heap' = newHeap node
        node  = hLookup global.heap addr
        newHeap nd = case nd of
            NAp a1 a2       -> hUpdate global.heap addr (NLAp a1 a2 local.taskid [])
            NGlobal n c 
                | n == 0    -> hUpdate global.heap addr (NLGlobal n c local.taskid [])
            _               -> global.heap

unlock :: Addr -> GmState -> GmState
unlock addr (global, local)
    = newState node
    where
        node  = hLookup global.heap addr
        newState nd = case nd of
            NLAp a1 a2 _tid ps
                -> emptyPendingList ps $ unlock a1 (global { heap = hUpdate global.heap addr (NAp a1 a2) }, local)
            NLGlobal n c _tid ps
                -> emptyPendingList ps $ (global { heap = hUpdate global.heap addr (NGlobal n c)}, local)
            _   -> (global, local)

step :: PgmGlobalState -> PgmLocalState -> GmState
step global local 
    = list (error "step: no code") phi local.code
    where
        phi i is = dispatch i (global, local { code = is })

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
    Par             -> pgmPar

unwind :: GmState -> GmState
unwind gl@(global, local)
    = newState (hLookup global.heap a)
    where
        (a, stk) = Stk.pop local.stack
        newState node = case node of
            NNum _
                | isEmptyStack local.dump
                    -> ( global
                       , local { ruleid = 10 }
                       )
                | otherwise
                    -> ( global
                       , local { code  = i'
                               , stack = Stk.push a stk'
                               , vstack = vstk'
                               , dump  = dump'
                               , ruleid = 22
                               }
                        )
                where
                    ((i',stk',vstk'), dump') = Stk.pop local.dump
            NAp a1 a2 -> ( global { heap = trace msg hUpdate global.heap a (NLAp a1 a2 local.taskid []) }
                         , local { code = [Unwind]
                                 , stack = stk'
                                 , ruleid = 52
                                 }
                         )
                where
                    stk' = Stk.push a1 local.stack
                    msg = "task#" ++ show local.taskid ++ " locks   #" 
                        ++ show a ++ " : " ++ iDisplay (showNode global a node)
            NInd a1   -> ( global
                         , local { code = [Unwind]
                                 , stack = Stk.push a1 stk
                                 , ruleid = 17
                                 }
                         )
            NGlobal n c
                | n == 0
                    -> ( global { heap = trace msg hUpdate global.heap a (NLGlobal n c local.taskid [])}
                       , local { code = c }
                       )
                | k < n
                    -> ( global 
                       , local { code  = i'
                               , stack = Stk.push ak stk'
                               , dump  = dump'
                               , vstack = vstk'
                               , ruleid = 29
                               }
                        )
                | otherwise
                    -> ( global
                       , local { code = c
                               , stack = rearrange n global.heap local.stack
                               , ruleid = 19
                               }
                       )
                where 
                    k      = stk.curDepth
                    (ak,_) = Stk.pop $ Stk.discard k local.stack
                    ((i',stk',vstk'), dump') = Stk.pop local.dump
                    msg = "task#" ++ show local.taskid ++ " locks   #" 
                        ++ show a ++ " : " ++ iDisplay (showNode global a node)
            NConstr _ _
                | isEmptyStack local.dump -> (global, local)
                | otherwise
                    -> ( global
                       , local { code = i'
                               , stack = Stk.push a stk'
                               , vstack = vstk'
                               , dump  = dump'
                               , ruleid = 35
                               }
                       )
                where
                    ((i',stk',vstk'), dump') = Stk.pop local.dump 
            NLAp _ _ _ pl -> ( global' { sparks = local' {code = [Unwind]} : pl }
                             , emptyTask )
                where
                    (global', local') = unlock a gl

            NLGlobal _ _ _ pl -> ( global' { sparks = local' {code = [Unwind]} : pl }
                                 , emptyTask )
                where
                    (global', local') = unlock a gl

pushGlobal :: GmGlobalMode -> GmState -> GmState
pushGlobal f (global, local) = case f of
    GlobalPack tag arity
        | show f `elem` aDomain global.globals
            -> ( global
               , local { stack = Stk.push a local.stack
                       , ruleid = 37
                       }
               )
        | otherwise
            -> ( global { heap = heap'
                        , globals = aInsert global.globals (show f) a
                        }
               , local { stack = Stk.push a local.stack
                       , ruleid = 38
                       }
               )
            where
                node = NGlobal arity [Pack tag arity, Update 0, Unwind]
                (heap', a') = hAlloc global.heap node
                a = aLookup global.globals (show f) a'
                
    GlobalLabel name -> ( global
                        , local { stack = Stk.push a local.stack
                                , ruleid = 5
                                }
                        )
        where
            a = aLookup global.globals name
                    (error $ "pushglobal: undeclared global: " ++ name)


pushInt :: Int -> GmState -> GmState
pushInt n (global, local)
    = case aLookup global.globals name (negate 1) of
        a' | a' < 0    -> ( global { heap    = heap'
                                   , globals = aInsert global.globals name a
                                   }
                          , local { stack = Stk.push a local.stack
                                  , ruleid = 14
                                  }
                          )
           | otherwise -> ( global
                          , local { stack = Stk.push a' local.stack
                                  , ruleid = 13
                                  }
                          )
    where
        name = show n
        node = NNum n
        (heap',a) = hAlloc global.heap node

push :: Int -> GmState -> GmState
push n (global, local)
    = ( global
      , local { stack = Stk.push an local.stack
              , ruleid = 18 
              }
      )
        where
            an = local.stack.stkItems !! n

pop :: Int -> GmState -> GmState
pop n (global, local)
    = ( global
      , local { stack = Stk.discard n local.stack
              , ruleid = 16
              }
      )

update :: Int -> GmState -> GmState
update n (global, local)
    = ( global' { heap = heap' }
      , local' { stack = stk' }
      )
    where
        (a, stk') = Stk.pop local.stack
        ra = stk'.stkItems !! n 
        (global',local') = unlock ra (global,local)
        heap' = hUpdate global'.heap ra (NInd a)

mkAp :: GmState -> GmState
mkAp (global, local)
    = ( global { heap = heap' }
      , local { stack = stk'
              , ruleid = 7 
             }
      )
    where
        (a1, stk1) = Stk.pop local.stack
        (a2, stk2) = Stk.pop stk1
        (heap', a) = hAlloc global.heap (NAp a1 a2)
        stk'       = Stk.push a stk2

slide :: Int -> GmState -> GmState
slide n (global, local)
    = ( global
      , local { stack = stk'
              , ruleid = 5
              }
      )
    where
        (a, stk) = Stk.pop local.stack
        stk' = Stk.push a (Stk.discard n stk)

alloc :: Int -> GmState -> GmState
alloc n (global, local)
    = ( global { heap = heap' }
      , local { stack = foldr Stk.push local.stack as
              , ruleid = 20
              }
      )
    where
        (heap', as) = allocNodes n global.heap

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes m heap = case m of
    0   -> (heap, [])
    n+1 -> (heap2, a:as)
        where
            (heap1, as) = allocNodes n heap
            (heap2, a ) = hAlloc heap1 (NInd hNull)
    _   -> error "allocNodes: negative number"

evalop :: GmState -> GmState
evalop (global, local)
    = case hLookup global.heap a of
        NNum _      -> (global, local)
        NConstr _ _ -> (global, local)
        _           -> (global
                       ,local { code   = [Unwind]
                              , stack  = Stk.push a emptyStack
                              , vstack = emptyStack
                              , dump   = Stk.push (local.code, stk, local.vstack) local.dump
                              , ruleid = 48
                              }
                       )
    where
        (a,stk) = Stk.pop local.stack

dyadicIntOp :: (Int -> Int -> Int) -> GmState -> GmState
dyadicIntOp bop (global, local)
    = ( global
      , local { vstack = Stk.push (a1 `bop` a2) vstk2
              , ruleid = 39 
              }
      )
    where
        (a1, vstk1) = Stk.pop local.vstack
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
unaryOp uop (global, local)
    = ( global
      , local { vstack = Stk.push (uop v) vstk'
              , ruleid = 40
              }
      )
    where
        (v, vstk') = Stk.pop local.vstack

cond :: GmCode -> GmCode -> GmState -> GmState
cond ti fi (global, local)
    = ( global
      , local { code   = cont ++ local.code
              , vstack = vstk'
              , ruleid = rid
              }
      )
    where
        (v,vstk') = Stk.pop local.vstack 
        (cont,rid) = case v of
            2 -> (ti,46)
            1 -> (fi,47)
            _ -> error "not boolean"

pack :: Tag -> Arity -> GmState -> GmState
pack tag arity (global, local)
    = ( global { heap = heap' }
      , local { stack = Stk.push a stk'
              , ruleid = 30
              }
      )
    where
        (as, stk') = Stk.npop arity local.stack
        (heap', a) = hAlloc global.heap (NConstr tag as)

caseJump :: [(Int, GmCode)] -> GmState -> GmState
caseJump alts (global, local)
    = ( global
      , local { code   = cont ++ local.code
              , ruleid = 31
              }
      )
    where
        (a, _) = Stk.pop local.stack
        cont = case hLookup global.heap a of
            NConstr t _
                -> aLookup alts t
                    (error $ "No case for constructor<" ++ show t ++ ">")
            n   -> error $ "casejump: Not data structure: " ++ show n

split :: Int -> GmState -> GmState
split n (global, local)
    = ( global
      , local { stack = stk' 
              , ruleid = 32
              }
      )
    where
        (a, stk) = Stk.pop local.stack
        stk' = case hLookup global.heap a of
            NConstr _ as
                | n == length as -> foldr Stk.push stk as
                | otherwise      -> error "Not saturated"
            _                    -> error "Not data structure"

pushBasic :: Int -> GmState -> GmState
pushBasic n (global, local)
    = ( global
      , local { vstack = Stk.push n local.vstack
              , ruleid = 41
              }
      )

mkBool :: GmState -> GmState
mkBool (global, local)
    = ( global { heap = heap' }
      , local { stack = Stk.push addr local.stack
              , vstack = vstk'
              , ruleid = 42
              }
      )
    where
        (tag, vstk') = Stk.pop local.vstack
        (heap',addr) = hAlloc global.heap (NConstr tag [])

mkInt  :: GmState -> GmState
mkInt (global, local)
    = ( global { heap = heap' }
      , local { stack = Stk.push addr local.stack
              , vstack = vstk'
              , ruleid = 43
              }
      )
    where
        (n,vstk') = Stk.pop local.vstack
        (heap',addr) = hAlloc global.heap (NNum n)

updateBool :: Int -> GmState -> GmState
updateBool n (global, local)
    = ( global'' { heap = heap' }
      , local'' { stack = stk' }
      )
    where
        (global',local') = mkBool (global,local)
        (a, stk') = Stk.pop local'.stack
        node = hLookup global'.heap a
        ra = stk'.stkItems !! n
        (global'',local'') = unlock ra (global',local')
        heap' = hUpdate global''.heap ra node

updateInt :: Int -> GmState -> GmState
updateInt n (global, local)
    = ( global'' { heap = heap' }
      , local'' { stack = stk' }
      )
    where
        (global', local') = mkInt (global, local)
        (a, stk') = Stk.pop local'.stack
        node = hLookup global'.heap a
        ra = stk'.stkItems !! n
        (global'',local'') = unlock ra (global',local')
        heap' = hUpdate global''.heap ra node

gmGet :: GmState -> GmState
gmGet (global, local)
    = ( global
      , local { stack  = stk'
              , vstack = Stk.push n local.vstack
              , ruleid = rid
              }
      )
    where
        (a,stk') = Stk.pop local.stack
        (n,rid) = case hLookup global.heap a of
            NConstr t [] -> (t,44)
            NNum m       -> (m,45)
            _            -> error "invalid node for Get"

gmReturn :: GmState -> GmState
gmReturn (global, local)
    = ( global
      , local { code = cont
              , stack = stk'
              , vstack = vstk
              , dump = dmp
              }
      )
    where
        ((cont,stk,vstk),dmp) = Stk.pop local.dump
        ak = last local.stack.stkItems
        stk' = Stk.push ak stk

gmPrint :: GmState -> GmState
gmPrint (global, local)
    = case hLookup global.heap a of
        NNum n
            -> ( global { output = show n }
               , local { stack  = stk
                       , ruleid = 33
                       }
               )
        NConstr t as 
            -> ( global { output = showConstr t (length as) }
               , local { code   = printCode (length as) ++ local.code
                       , stack  = foldr Stk.push stk as
                       , ruleid = 34
                       }
               )
        _   -> error "gmprint: cannot print"
    where
        (a, stk) = Stk.pop local.stack
        showConstr tag arity
            = "Pack{" ++ show tag ++ "," ++ show arity ++ "}"
        printCode n = concat $ take n $ cycle [[Eval, Print]]

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stk
    = foldr phi (Stk.discard n stk) $ take n $ drop 1 stk.stkItems
    where
        phi a = Stk.push (getArg (hLookup heap a))

getArg :: Node -> Addr
getArg (NAp _ a2)      = a2
getArg (NLAp _ a2 _ _) = a2
getArg (NInd a)        = a
getArg n               = error $ "getArg: Not application node: " ++ show n

pgmPar :: GmState -> GmState
pgmPar (global, local)
    = ( global { sparks = makeTask global.maxtid a : global.sparks
               , maxtid  = succ global.maxtid  }
      , local  { stack  = stk
               , ruleid = 51 
               }
      )
    where
        (a, stk) = Stk.pop local.stack

