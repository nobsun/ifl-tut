module TIM.Mark2.Machine
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

import TIM.Mark2.Code
import TIM.Mark2.Frame
import TIM.Mark2.PPrint
import TIM.Mark2.State

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
    = [ ("+", [ Take 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("-", [ Take 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("*", [ Take 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("/", [ Take 2
              , Push (Code [ Push (Code [Op Add, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("negate", [ Take 1
                   , Push (Code [Op Neg, Return])
                   , Enter (Arg 1) ])
      , (">", [ Take 2
              , Push (Code [ Push (Code [Op Gt, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , (">=", [ Take 2
               , Push (Code [ Push (Code [Op Ge, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("<", [ Take 2
              , Push (Code [ Push (Code [Op Lt, Return])
                           , Enter (Arg 1) ])
              , Enter (Arg 2) ])
      , ("<=", [ Take 2
               , Push (Code [ Push (Code [Op Le, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("==", [ Take 2
               , Push (Code [ Push (Code [Op Eq, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("/=", [ Take 2
               , Push (Code [ Push (Code [Op Ne, Return])
                           , Enter (Arg 1) ])
               , Enter (Arg 2) ])
      , ("if", [ Take 3
               , Push (Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]])
               , Enter (Arg 1)])
      ]

type TimCompilerEnv = Assoc Name TimAMode

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, Code)
compileSC env (name, args, body)
    | null args = (name, code)
    | otherwise = (name, Take (length args) : code)
    where
        code = compileR body newEnv
        newEnv = zip args (map Arg [1 ..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> Code
compileR e env = case e of
    EAp e1 e2
        | isBinOp e -> compileB e env [Return]
        | isIf e    -> case e of
            EAp (EAp (EAp (EVar _) cond) tclause) eclause
                -> case compileR tclause env of
                    itc -> case compileR eclause env of
                        etc -> compileB cond env [Cond itc etc]
            _       -> error "compileR: unexpected if-expression"
        | otherwise -> Push (compileA e2 env) : compileR e1 env
    EVar _v   -> [Enter (compileA e env)]
    ENum n    -> [PushV (IntVConst n), Return]
    _         -> error "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA e env = case e of
    EVar v -> aLookup env v (error ("compileA: unknown variable " ++ show v))
    ENum n -> IntConst n
    _      -> Code (compileR e env)

compileB :: CoreExpr -> TimCompilerEnv -> Code -> Code
compileB e env cont
    = case e of
        ENum n -> PushV (IntVConst n) : cont
        EAp (EAp (EVar o) e1) e2
            | isBinOpName o -> compileB e2 env (compileB e1 env (Op (nameToOp o) : cont))
        _      -> Push (Code cont) : compileR e env

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
isBinOpName name = isJust (lookup name primitives)

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
    Take n : instr
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
            stack' = Stk.discard n state'.stack
            (heap', fptr') = fAlloc state'.heap (Frame $ take n state'.stack.stkItems)
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
    
