module TIM.Mark1gc.Machine
    where

import Data.Char
import Data.List

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import TIM.Mark1gc.Code
import TIM.Mark1gc.Frame
import TIM.Mark1gc.PPrint
import TIM.Mark1gc.State

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

run :: (?sz :: Int, ?th :: Int) => String -> ([String] -> [String])
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
    , frPtr     = FrameNull
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
initialArgStack = emptyStack

initialValueStack :: TimValueStack
initialValueStack = DummyTimValueStack

initialDump :: TimDump
initialDump = DummyTimDump

compiledPrimitives :: Assoc Name Code
compiledPrimitives = []

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
    EAp e1 e2 -> Push (compileA e2 env) : compileR e1 env
    EVar _v   -> [Enter (compileA e env)]
    ENum _n   -> [Enter (compileA e env)]
    _         -> error "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA e env = case e of
    EVar v -> aLookup env v (error ("compileA: unknown variable " ++ show v))
    ENum n -> IntConst n
    _      -> Code (compileR e env)

--

eval :: (?sz :: Int, ?th :: Int) => TimState -> [TimState]
eval state = state : rests
    where
        rests | timFinal state = []
              | otherwise      = eval state'
        state' = doAdmin (step state)

doAdmin :: (?sz :: Int, ?th :: Int) => TimState -> TimState
doAdmin = gc . applyToStats statIncSteps

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
                      , frPtr = fptr'
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
                      , frPtr = fptr'
                      }
        _   -> error "step: invalid code sequence"
        where
            (instr', fptr') = amToClosure am state'.frPtr state'.heap state'.codestore
    Push am : instr
        -> countUpExtime
        $  state' { code = instr
                  , stack = Stk.push clos state'.stack
                  }
        where
            clos = amToClosure am state'.frPtr state'.heap state'.codestore
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
intCode = []

gc :: (?sz :: Int, ?th :: Int) => TimState -> TimState
gc state = case evacuate state.heap hInitial state.frPtr state.stack of
    ((from', to'), frptr', stack') -> case scavenge from' to' of
        heap' -> state { frPtr = frptr'
                       , stack = stack'
                       , heap = heap'
                       }

evacuate :: TimHeap -> TimHeap -> FramePtr -> TimStack -> ((TimHeap, TimHeap), FramePtr, TimStack)
evacuate from to fp stack = case mapAccumL evacuateFrom (from, to) (fp : map snd stack.stkItems) of
    (heaps', fp':fps') -> (heaps', fp', stack { stkItems = zip (map fst stack.stkItems) fps'})
    _                  -> error "evacuate: invalid new frame pointers"

evacuateFrom :: (TimHeap, TimHeap) -> FramePtr -> ((TimHeap, TimHeap), FramePtr)
evacuateFrom (from, to) fp = case fp of
    FrameAddr a    -> case hLookup from a of
        Frame clos      -> case mapAccumL evacuateFrom (from, to) (map snd clos) of
            ((from', to'), fps') -> case fAlloc to' newFrame of
                (to'', fp'')          -> case fp'' of
                    FrameAddr a''       -> trace "FORWARD" ((hUpdate from' a (Forward a''), to''), fp'')
                    _                   -> ((from', to''), fp'')
                where
                    newFrame = Frame (zip (map fst clos) fps')
        Forward _       -> ((from, to), fp)
    _              -> ((from, to), fp)

scavenge :: TimHeap -> TimHeap -> TimHeap
scavenge _from to = foldl' phi to to.assocs
    where
        phi t (a, f) = case f of
            Forward a' -> trace "UPDATE" hUpdate t a (hLookup t a')
            _          -> t
