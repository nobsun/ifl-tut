module TIM.Mark3.Machine
    where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard, emptyStack)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import TIM.Mark3.Code
import TIM.Mark3.Frame
import TIM.Mark3.PPrint
import TIM.Mark3.State

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
    = showResults 
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
    , code      = [Enter (Label "main")]
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
initialArgStack = Stk.push ([], FrameNull) Stk.emptyStack

initialValueStack :: TimValueStack
initialValueStack = Stk.emptyStack

initialDump :: TimDump
initialDump = DummyTimDump

compiledPrimitives :: Assoc Name Code
compiledPrimitives 
    = [ ("+", [ Take 2 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("-", [ Take 2 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("*", [ Take 2 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("/", [ Take 2 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("negate", [ Take 1 1
                   , Push (Code [Op Neg, Return])
                   , Enter (Arg 1) ])
      , (">", [ Take 2 2
              , Push (Code [ Push (Code [Op Gt, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , (">=", [ Take 2 2
               , Push (Code [ Push (Code [Op Ge, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("<", [ Take 2 2
              , Push (Code [ Push (Code [Op Lt, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("<=", [ Take 2 2
               , Push (Code [ Push (Code [Op Le, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("==", [ Take 2 2
               , Push (Code [ Push (Code [Op Eq, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("/=", [ Take 2 2
               , Push (Code [ Push (Code [Op Ne, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("if", [ Take 3 3
               , Push (Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]])
               , Enter (Arg 1)])
      ]

type TimCompilerEnv = Assoc Name TimAMode

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, Code)
compileSC env (name, args, body)
    | null args = (name, code)
    | otherwise = (name, Take d n : code)
    where
        n = length args
        (d, code) = compileR body newEnv n
        newEnv = zip args (map Arg [1 ..]) ++ env

type OccupiedSlots = Int

compileR :: CoreExpr -> TimCompilerEnv -> OccupiedSlots-> (OccupiedSlots, Code)
compileR e env d = case e of
    ELet isRec defns body
        -> (d', moves ++ il)
        where
            n = length defns
            (dn, moves) = mapAccumL moveInstr (d+n) (zip defns slots)
            (d', il)    = compileR body env' dn
            env'        = zip (map fst defns) (map mkIndMode slots) ++ env
            slots       = [d+1 ..]
            moveInstr i ((_, rhs), k) = (d1, Move k am)
                where
                    (d1, am) = compileA rhs rhsenv i
                    rhsenv | isRec     = env'
                           | otherwise = env

    EAp e1 e2
        | isBinOp e -> compileB e env (d, [Return])
        | isIf e    -> case e of
            EAp (EAp (EAp (EVar _) cond) tclause) eclause
                -> case compileR tclause env d of
                    (d1', itc) -> case compileR eclause env d of
                        (d2', etc) -> compileB cond env (max d1' d2', [Cond itc etc])
            _       -> error "compileR: unexpected if-expression"
        | otherwise -> (d2, Push am : il)
            where
                (d1, am) = compileA e2 env d
                (d2, il) = compileR e1 env d1
    EVar _v   -> (d1, [Enter am])
        where
            (d1, am) = compileA e env d
    ENum n    -> (d, [PushV (IntVConst n), Return])
    _         -> error "compileR: can't do this yet"

mkIndMode :: Int -> TimAMode
mkIndMode i = Code [Enter (Arg i)]

compileA :: CoreExpr -> TimCompilerEnv -> OccupiedSlots -> (OccupiedSlots, TimAMode)
compileA e env d = case e of
    EVar v -> (d, aLookup env v (error ("compileA: unknown variable " ++ show v)))
    ENum n -> (d, IntConst n)
    _      -> (d1, Code il)
        where
            (d1, il) = compileR e env d

compileB :: CoreExpr -> TimCompilerEnv -> (OccupiedSlots, Code) -> (OccupiedSlots, Code)
compileB e env (d, cont)
    = case e of
        ENum n -> (d, PushV (IntVConst n) : cont)
        EAp (EAp (EVar o) e1) e2
            | isBinOpName o -> compileB e2 env (compileB e1 env (d, Op (nameToOp o) : cont))
        EAp (EVar u) e1
            | isUniOpName u -> compileB e1 env (d, Op (nameToOp u) : cont)
        _      -> (d1, Push (Code cont) : il)
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

             -- | Eq | Ne | Lt | Le | Gt | Ge

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
timFinal state = null state.code || null state.ctrl

applyToStats :: (TimStats -> TimStats) -> (TimState -> TimState)
applyToStats f state = state { stats = f state.stats }

countUpExtime :: TimState -> TimState
countUpExtime = applyToStats statIncExtime

countUpHpAllocs :: Int -> TimState -> TimState
countUpHpAllocs n = applyToStats (statIncHpAllocs (n+1))

step :: TimState -> TimState
step state = case state'.code of
    []  -> error "step: the state is already final"
    Take t n : instr
        | state.stack.curDepth >= n 
            -> countUpHpAllocs n
            $  state' { code = instr
                      , frame = fptr'
                      , stack = stack'
                      , heap = heap' 
                      }
        | otherwise 
            -> error "step: Too few args for Take instruction"
        where
            (cs, stack') = Stk.npop n state'.stack
            (heap', fptr') = fAlloc state'.heap (cs ++ replicate (t - n) ([], FrameNull))

    Move n am : instr
        -> countUpHpAllocs n
        $  state' { code = instr
                  , heap = heap' 
                  }
        where
            heap' = fUpdate state'.heap fptr n (amToClosure am fptr state'.heap state'.codestore)
            fptr  = state'.frame

    Enter am : instr -> case instr of
        []  -> countUpExtime
            $  state' { code = instr'
                      , frame = fptr'
                      }
        _   -> error "step: invalid code sequence"
        where
            (instr', fptr') = amToClosure am state'.frame state'.heap state'.codestore
    Push am : instr
        -> countUpExtime
        $  state' { code = instr
                  , stack = Stk.push clos state'.stack
                  }
        where
            clos = amToClosure am state'.frame state'.heap state'.codestore
    Op Add : instr
        -> countUpExtime
        $  state' { code   = instr
                  , vstack = Stk.push (n1 + n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Sub : instr
        -> countUpExtime
        $  state' { code   = instr
                  , vstack = Stk.push (n1 - n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Mul : instr
        -> countUpExtime
        $  state' { code   = instr
                  , vstack = Stk.push (n1 * n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Div : instr
        -> countUpExtime
        $  state' { code   = instr
                  , vstack = Stk.push (n1 `div` n2) vstack'
                  }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Eq : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 == n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Ne : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 /= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Lt : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 < n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Le : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 <= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Gt : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 > n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Op Ge : instr
        -> countUpExtime
        $ state' { code = instr
                 , vstack = Stk.push (bool 1 0 (n1 >= n2)) vstack'
                 }
        where
            (n1,n2)      = (vs !! 0, vs !! 1)
            (vs,vstack') = Stk.npop 2 state'.vstack
    Return : []
        -> state' { code = instr' 
                  , frame = fptr'
                  , stack = stack'
                  }
        where
            ((instr',fptr'), stack') = Stk.pop state.stack
    PushV FramePtr : instr
        -> state' { code = instr
                  , vstack = Stk.push n state.vstack
                  }
        where
            n = case state.frame of
                FrameInt m -> m
                _          -> error "invalid frame pointer in the case"
    PushV (IntVConst n) : instr
        -> state' { code = instr
                  , vstack = Stk.push n state.vstack
                  }
    Cond i1 i2 : []
        -> countUpExtime
        $  state' { code = if v == 0 then i1 else i2
                  , vstack = vstack'
                  }
        where
            (v,vstack') = Stk.pop state.vstack
    c:_ -> trace (show c) undefined
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

intCode :: Code
intCode = [PushV FramePtr, Return]
    
