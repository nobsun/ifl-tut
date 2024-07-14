{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TIM.Mark4.Machine
    where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

import Language
import Heap
import qualified Stack as Stk (push, append, top, pop, npop, discard, emptyStack, isEmptyStack)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import TIM.Mark4.Code
import TIM.Mark4.Frame
import TIM.Mark4.PPrint
import TIM.Mark4.State

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

--

run :: String -> ([String] -> [String])
run prog inputs
    = showFullResults 
    $ eval 
    $ setControl inputs
    $ compile 
    $ parse prog

setControl :: [String] -> TimState -> TimState
setControl ctrl state = state { ctrl = ctrl }

--

defaultHeapSize :: Int
defaultHeapSize = 1024 ^ (2 :: Int)

defaultThreshold :: Int
defaultThreshold = 50

--

compile :: CoreProgram -> TimState
compile program = TimState
    { ctrl      = []
    , code      = CCode [] [Enter (Label "main")]
    , frame     = FrameNull
    , stack     = initialArgStack
    , vstack    = initialValueStack
    , dump      = initialDump
    , heap      = let ?sz = defaultHeapSize
                      ?th = defaultThreshold
                  in hInitial
    , codestore = compiledCode
    , stats     = statInitial
    , ruleid    = 0
    }
    where
        compiledCode = compiledScDefs ++ compiledPrimitives
        compiledScDefs = map (compileSC initialEnv) scDefs
        scDefs = preludeDefs ++ program
        initialEnv = [(name, Label name) | (name, _args, _body) <- scDefs ]
                  ++ [(name, Label name) | (name, _code) <- compiledPrimitives ]

initialArgStack :: TimStack
initialArgStack = Stk.push (CCode [] [], FrameNull) Stk.emptyStack

initialValueStack :: TimValueStack
initialValueStack = Stk.emptyStack

initialDump :: TimDump
initialDump = Stk.emptyStack

compiledPrimitives :: Assoc Name CCode
compiledPrimitives
    = (compilePrimitive <$> primitives)
    ++ [ ("if"
       , CCode [1,2,3] 
            [ Take 3 3
            , Push (Code (CCode [2,3] 
                [ Cond (CCode [2] [Enter (Arg 2)])
                       (CCode [3] [Enter (Arg 3)])
                ]))
            , Enter (Arg 1)])
      ]

compilePrimitive :: (Name, Op) -> (Name, CCode)
compilePrimitive (name,op) = (name,) $ case op of
    Neg -> CCode [1]
        [ Take 1 1
        , Push (Code (CCode [] 
            [Op Neg, Return]))
        , Enter (Arg 1) 
        ]
    _bop -> CCode [1,2]
        [ Take 2 2
        , Push (Code (CCode [1]
            [ Push (Code (CCode [] [Op op, Return]))
            , Enter (Arg 1)
            ]))
        , Enter (Arg 2)
        ]

type TimCompilerEnv = Assoc Name TimAMode

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, CCode)
compileSC env (name, args, body)
    | d == 0    = (name, ccode)
    | otherwise = (name, CCode ss $ Take d n : code)
    where
        n = length args
        (d, ccode@(CCode ss code)) = compileR body env' n
        env' = zip args (map Arg [1 ..]) ++ env

type OccupiedSlots = Int

compileR :: CoreExpr -> TimCompilerEnv -> OccupiedSlots-> (OccupiedSlots, CCode)
compileR e env d = case e of
    ELet isRec defns body
        -> (d', CCode (merge ns ns') (concat ils ++ il))
        where
            n = length defns
            ns' = foldr merge [] nss
            (nss, ils) = unzip $ (\ (CCode s is) -> (s,is)) <$> moves
            (dn, moves) = mapAccumL moveInstr (d+n) (zip defns slots)
            (d', CCode ns il) = compileR body env' dn
            env'        = zip (map fst defns) (map mkIndMode slots) ++ env
            slots       = [d+1 ..]
            moveInstr i ((_, rhs), k) = (d1, CCode ss [Move k am])
                where
                    (d1, am) = compileAL rhs d rhsenv i
                    ss = case am of
                        Code ccode -> ccode.slots
                        _          -> error "AMode is not Code"
                    rhsenv | isRec     = env'
                           | otherwise = env
    EAp e1 e2
        | isBinOp e -> compileB e env (d, CCode [] [Return])
        | isIf e    -> case e of
            EAp (EAp (EAp (EVar _) cond) tclause) eclause
                -> case compileR tclause env d of
                    (d1', itc) -> case compileR eclause env d of
                        (d2', etc) -> compileB cond env (max d1' d2', CCode (merge itc.slots etc.slots) [Cond itc etc])
            _       -> error "compileR: unexpected if-expression"
        | isAtomicExpr e2 
            -> let
                { (dn, il') = compileR e1 env d
                ; (_, il'') = compileA e2 env d
                }
               in (dn, CCode il.slots (Push il'' : il'.code))
        | otherwise 
            -- -> ( d2, CCode (merge ss il.slots) $ Push am : il.code)
            -> ( d2
               , CCode ss' (Move d' am : Push (Code (CCode [1] [Enter (Arg d')])) : il.code )
               )
            where
                d' = succ d
                (d1, am) = compileAL e2 d' env d' 
                ss = case am of
                    Code ccode -> ccode.slots
                    _          -> error "AMode is not Code"
                ss' = merge ss il.slots
                (d2, il) = compileR e1 env d1
    EVar _v   -> (d1, CCode ss (mkEnter am))
        where
            (d1, am) = compileA e env d
            ss = case am of
                Code ccode -> ccode.slots
                _          -> error "Amode is not Code"
    ENum n    -> (d, CCode [] [PushV (IntVConst n), Return])
    _         -> error "compileR: can't do this yet"

mkIndMode :: Int -> TimAMode
mkIndMode i = Code (CCode [i] [Enter (Arg i)])

mkUpdIndMode :: Int -> TimAMode
mkUpdIndMode i = Code (CCode [i] [PushMarker i, Enter (Arg i)])

mkEnter :: TimAMode -> [Instruction]
mkEnter am = case am of
    Code ccode -> ccode.code
    _          -> [Enter am]

compileA :: CoreExpr -> TimCompilerEnv -> OccupiedSlots -> (OccupiedSlots, TimAMode)
compileA e env d = case e of
    EVar v -> (d, aLookup env v (error ("compileA: unknown variable " ++ show v)))
    ENum n -> (d, IntConst n)
    _      -> (d1, Code il)
        where
            (d1, il) = compileR e env d

compileAL :: CoreExpr -> Int -> TimCompilerEnv -> OccupiedSlots -> (OccupiedSlots, TimAMode)
compileAL e upd env d = case e of
    ENum n -> (d, IntConst n)
    _      -> (d1, Code (CCode ([upd] `merge` s) (PushMarker upd : il)))
        where
            (d1, CCode s il) = compileR e env d

compileB :: CoreExpr -> TimCompilerEnv -> (OccupiedSlots, CCode) -> (OccupiedSlots, CCode)
compileB e env (d, cont)
    = case e of
        ENum n -> (d, CCode cont.slots (PushV (IntVConst n): cont.code))
        EAp (EAp (EVar o) e1) e2
            | isBinOpName o -> (max d1 d2, c2) 
                where
                    (d1, c1) = compileB e1 env (d, CCode cont.slots (Op (nameToOp o) : cont.code))
                    (d2, c2) = compileB e2 env (d, c1)
        EAp (EVar u) e1
            | isUniOpName u -> compileB e1 env (d, CCode cont.slots (Op (nameToOp u) : cont.code))
        _      -> (d1, CCode (merge cont.slots il.slots) (Push (Code cont) : il.code))
            where
                (d1, il) = compileR e env d

decompose :: CoreExpr -> (CoreExpr, Name, CoreExpr)
decompose expr = case expr of
    EAp (EAp (EVar o) exp1) exp2 -> (exp1,o,exp2)
    _                            -> error "compileB: not binop"

isIf :: CoreExpr -> Bool
isIf e = case e of
    EAp (EAp (EAp (EVar "if") _) _) _ -> True
    _                                 -> False

isBinOp :: CoreExpr -> Bool
isBinOp e = case e of
    EAp (EAp (EVar v) _) _
        | isBinOpName v -> True
    _                   -> False

isBinOpName :: Name -> Bool
isBinOpName name = isJust (lookup name primitives) && notElem name uniOps

isUniOpName:: Name -> Bool
isUniOpName name = name `elem` uniOps

uniOps :: [Name]
uniOps = ["negate"]

nameToOp :: Name -> Op
nameToOp name = aLookup primitives name (error "nameToOp: no op")

primitives :: Assoc Name Op
primitives = [("+", Add)
             ,("-", Sub)
             ,("*", Mul)
             ,("/", Div)
             ,("==", Eq)
             ,("/=", Ne)
             ,("<", Lt)
             ,("<=", Le)
             ,(">", Gt)
             ,(">=", Ge)
             ,("negate",Neg)
             ]

--

eval :: TimState -> [TimState]
eval state = state : rests
    where
        rests | timFinal state = []
              | otherwise      = eval state'
        state' = doAdmin (step state)

doAdmin :: TimState -> TimState
doAdmin = applyToStats statIncSteps

timFinal :: TimState -> Bool
timFinal state = null state.code.code || null state.ctrl

applyToStats :: (TimStats -> TimStats) -> (TimState -> TimState)
applyToStats f state = state { stats = f state.stats }

countUpExtime :: TimState -> TimState
countUpExtime = applyToStats statIncExtime

countUpHpAllocs :: Int -> TimState -> TimState
countUpHpAllocs n = applyToStats (statIncHpAllocs (n+1))

step :: TimState -> TimState
step state = case state'.code of
    CCode _ []  -> error "step: the state is already final"
    CCode ss (Take t n : instr)
        | state.stack.curDepth >= n 
            -> countUpHpAllocs n
            $  state' { code = CCode ss instr
                      , frame = fptr'
                      , stack = stack'
                      , heap = heap' 
                      }
        | otherwise 
            -> error "step: Too few args for Take instruction"
        where
            (cs, stack') = Stk.npop n state'.stack
            (heap', fptr') = fAlloc state'.heap (Frame $ cs ++ replicate (t - n) (CCode [] [], FrameNull))
    CCode ss (Move n am : instr)
        -> countUpHpAllocs n
        $  state' { code = CCode ss instr
                  , heap = heap' 
                  }
        where
            heap' = fUpdate state'.heap fptr n (amToClosure am fptr state'.heap state'.codestore)
            fptr  = state'.frame
    CCode ss (Push am : instr)
        -> countUpExtime
        $  state' { code = CCode ss instr
                  , stack = Stk.push clos state'.stack
                  }
        where
            clos = amToClosure am state'.frame state'.heap state'.codestore
    CCode ss (PushV FramePtr : instr)
        -> countUpExtime
        $  state' { code = CCode ss instr
                  , vstack = Stk.push n state.vstack
                  }
        where
            n = case state.frame of
                FrameInt m -> m
                _          -> error "invalid frame pointer in the case"
    CCode ss (PushV (IntVConst n) : instr)
        -> countUpExtime
        $  state' { code = CCode ss instr
                  , vstack = Stk.push n state.vstack
                  }
    CCode ss (PushMarker x : i)
        -> countUpExtime
        $  state' { code = CCode ss i
                  , stack = empty state'.stack
                  , dump  = Stk.push (TimDumpItem state'.frame x state'.stack) state'.dump
                  }
    CCode ss (UpdateMarkers n : i)
        | n <= state'.stack.curDepth
            -> countUpExtime
            $  state' { code = CCode ss i }
        | otherwise
            -> countUpExtime
            $  state' { stack = Stk.append state'.stack clos
                      , heap  = heap2
                      }
        where
            (heap1, paFptr) = fAlloc state'.heap (Frame state'.stack.stkItems)
            TimDumpItem fUpd x clos = Stk.top state'.dump
            heap2 = fUpdate heap1 fUpd x (paCode, paFptr)
            paCode = CCode ms (map (Push . Arg) (reverse ms))
                where
                    stk = state'.stack
                    m   = stk.curDepth :: Int
                    ms  = [1 .. m ]

    CCode _ (Enter am : instr) -> case instr of
        []  -> countUpExtime
            $  state' { code = instr'
                      , frame = fptr'
                      }
        _   -> error "step: invalid code sequence"
        where        
            (instr', fptr') = amToClosure am state'.frame state'.heap state'.codestore
    CCode _ (Return : [])
        | Stk.isEmptyStack state'.stack 
            -> case Stk.pop state'.dump of
                (TimDumpItem fu x s, dump')
                    -> countUpExtime 
                    $ state' { stack = s
                             , heap  = heap' 
                             , dump  = dump'
                             }
                    where
                        heap'  = fUpdate state'.heap fu x (intCode, FrameInt n)
                        (n, _) = Stk.pop state'.vstack
        | otherwise
            -> countUpExtime
            $  state' { code = instr' 
                      , frame = fptr'
                      , stack = stack'
                      }
            where
                ((instr',fptr'), stack') = Stk.pop state'.stack
    CCode ss (Op Add : instr)
        -> countUpExtime
        $  state' { code   = CCode ss instr
                  , vstack = Stk.push (n1 + n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Sub : instr)
        -> countUpExtime
        $  state' { code   = CCode ss instr
                  , vstack = Stk.push (n1 - n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Mul : instr)
        -> countUpExtime
        $  state' { code   = CCode ss instr
                  , vstack = Stk.push (n1 * n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Div : instr)
        -> countUpExtime
        $  state' { code   = CCode ss instr
                  , vstack = Stk.push (n1 `div` n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Eq : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 == n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Ne : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 /= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Lt : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 < n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Le : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 <= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Gt : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 > n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode ss (Op Ge : instr)
        -> countUpExtime
        $ state' { code = CCode ss instr
                 , vstack = Stk.push (bool 1 0 (n1 >= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    CCode _ (Cond i1 i2 : [])
        -> countUpExtime
        $  state' { code = if v == 0 then i1 else i2
                  , vstack = vstack'
                  }
        where
            (v,vstack') = Stk.pop state.vstack
    CCode _ (c:_) -> trace (show c) undefined
    where
        state' = ctrlStep state

ctrlStep :: TimState -> TimState
ctrlStep state = case state.ctrl of
    []     -> error "ctrlStep: already finished"
    c:cs -> case c of
        ""                -> state { ctrl = cs }
        "c"               -> state { ctrl = repeat "" }
        s | all isDigit s -> state { ctrl = replicate (read s) "" ++ cs }
          | otherwise     -> state { ctrl = cs }

        
amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure amode fptr heap cstore = case amode of
    Arg n      -> fGet heap fptr n
    Code il    -> (il, fptr)
    Label l    -> (codeLookup cstore l, fptr)
    IntConst n -> (intCode, FrameInt n)

intCode :: CCode
intCode = CCode [] [PushV FramePtr, Return]
    
{-
[("I",[Take 1 1,Enter (Arg 1)]),("K",[Take 2 2,Enter (Arg 1)]),("K1",[Take 2 2,Enter (Arg 2)]),("S",[Take 3 3,Push (Code [Push (Arg 3),Enter (Arg 2)]),Push (Arg 3),Enter (Arg 1)]),("compose",[Take 3 3,Push (Code [Push (Arg 3),Enter (Arg 2)]),Enter (Arg 1)]),("twice",[Take 1 1,Push (Arg 1),Push (Arg 1),Enter (Label "compose")]),



("f",[Take 3 1
    ,Move 2 (Code [PushV (IntVConst 0)
                  ,Push (Code [Op Eq
                              ,Cond [PushV (IntVConst 1),Return] [Enter (Code [Enter (Arg 3)])]
                              ])
                  ,Enter (Arg 1)])
    ,Move 3 (Code [PushV (IntVConst 0)
                  ,Push (Code [Op Eq,Cond [Enter (Code [Enter (Arg 2)])] [PushV (IntVConst 2),Return]
                              ])
                  ,Enter (Arg 1)])
    ,Push (Code [Push (Code [Op Add,Return])
                ,Enter (Code [Enter (Arg 2)])
                ])
    ,Enter (Code [Enter (Arg 3)])])





,("+",[Take 2 2,Push (Code [Push (Code [Op Add,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("-",[Take 2 2,Push (Code [Push (Code [Op Add,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("*",[Take 2 2,Push (Code [Push (Code [Op Add,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("/",[Take 2 2,Push (Code [Push (Code [Op Add,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("negate",[Take 1 1,Push (Code [Op Neg,Return]),Enter (Arg 1)]),(">",[Take 2 2,Push (Code [Push (Code [Op Gt,Return]),Enter (Arg 1)]),Enter (Arg 2)]),(">=",[Take 2 2,Push (Code [Push (Code [Op Ge,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("<",[Take 2 2,Push (Code [Push (Code [Op Lt,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("<=",[Take 2 2,Push (Code [Push (Code [Op Le,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("==",[Take 2 2,Push (Code [Push (Code [Op Eq,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("/=",[Take 2 2,Push (Code [Push (Code [Op Ne,Return]),Enter (Arg 1)]),Enter (Arg 2)]),("if",[Take 3 3,Push (Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]),Enter (Arg 1)])]
-}
