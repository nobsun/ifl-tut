module TIM.Mark1gc.Machine
    where

import Data.Char
import Data.List
import Data.Maybe

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
import TIM.Mark1gc.GC

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
            (instr', fptr') = trace "L159" amToClosure am state'.frame state'.heap state'.codestore
    Push am : instr
        -> countUpExtime
        $  state' { code = instr
                  , stack = Stk.push clos state'.stack
                  }
        where
            clos = trace "L166" amToClosure am state'.frame state'.heap state'.codestore
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
amToClosure amode fptr heap cstore = trace (">>>" ++ show amode) $ case amode of
    Arg n      -> fGet heap fptr n
    Code il    -> (il, fptr)
    Label l    -> (codeLookup cstore l, fptr)
    IntConst n -> (intCode, FrameInt n)

intCode :: Code
intCode = []

{-
type TimStack = Stack Closure
type TimHeap  = Heap Frame
type Frame    = [Closure]
type Closure  = (Code, FramePtr)
type Code     = [Instruction]
-}
