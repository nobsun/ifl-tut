module Template.Mark1 where

import Data.List

import Utils
import Heap
import Iseq
import Language

-- Mark 1 : A minimal template instatioation graph reducer

--- Structure of the implementation

run :: String -> String
run = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump deriving Show

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node
data Node = NAp Addr Addr                   -- ^ Application
          | NSupercomb Name [Name] CoreExpr -- ^ Supercombinator
          | NNum Int                        -- ^ Number
          deriving Show

type TiGlobals = Assoc Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = case state of
  (stack, dump, heap, scDefs, stats) -> (stack, dump, heap, scDefs, f stats)

-- compiler

compile :: CoreProgram -> TiState
compile prog
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = prog ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = [addressOfMain]
      addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccumL allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
  (name, args, body) -> (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

-- evaluator

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal state = case state of
  ([soleAddr], _, heap, _, _) -> isDataNode (hLookup heap soleAddr)
  ([], _, _, _, _)            -> error "Empty stack!"
  _                           -> False

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _ -> True
  _      -> False

step :: TiState -> TiState
step state = case state of
  (stack, dump, heap, globals, stats) -> dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = scStep  state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (stack, dump, heap, globals, stats) -> (a1:stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (stack, dump, heap, globals, stats) -> if length stack < length argNames + 1 then error "Too few argments given"
                                         else (stack', dump, heap', globals, stats)
    where
      stack' = resultAddr : drop (length argNames + 1) stack
      (heap', resultAddr) = instantiate body heap env
      env = argBindings ++ globals
      argBindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case stack of
  sc:stack' -> map getarg stack'
    where
      getarg addr = arg
        where
          NAp fun arg = hLookup heap addr
  []       -> error "Empty stack"


instantiate :: CoreExpr         -- Body of suprercombinator
            -> TiHeap           -- Heap before instatiation
            -> Assoc Name Addr  -- Association of names to address
            -> (TiHeap, Addr)   -- Heap after instatiation, and address of root of instance
instantiate expr heap env = case expr of
  ENum n               -> hAlloc heap  (NNum n)
  EAp e1 e2            -> hAlloc heap2 (NAp a1 a2)
    where
      (heap1, a1) = instantiate e1 heap  env
      (heap2, a2) = instantiate e2 heap1 env 
  EVar v               -> (heap, aLookup env v (error ("Undefined name " ++ show v)))
  EConstr tag arity    -> instantiateConstr tag arity heap env
  ELet isrec defs body -> instantiateLet isrec defs body heap env
  ECase e alts         -> error "Can't instantiate case exprs"
  ELam vs e            -> error "Can't instantiate lambda abstractions"

instantiateConstr tag arity heap env
  = error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env
  = error "Can't instantiate let(rec)s yet"

-- Formatting the results

showResults :: [TiState] -> String
showResults states
  = iDisplay (iConcat [ iLayn (map showState states)
                      , showStats (last states)
                      ])

showState :: TiState -> IseqRep
showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> IseqRep
showStack heap stack
  = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem stack))
    , iStr " ]"
    ]
    where
      showStackItem addr
        = iConcat [ showFWAddr addr, iStr ": "
                  , showStkNode heap (hLookup heap addr)
                  ]

showStkNode :: TiHeap -> Node -> IseqRep
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr
            , iStr " ", showFWAddr argAddr, iStr " ("
            , showNode (hLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode node

showNode :: Node -> IseqRep
showNode node = case node of
  NAp a1 a2 -> iConcat [ iStr "NAp ", showAddr a1
                       , iStr " ",    showAddr a2
                       ]
  NSupercomb name args body
            -> iStr ("NSupercomb " ++ name)
  NNum n    -> iStr "NNum " `iAppend` iNum n

showAddr :: Addr -> IseqRep
showAddr addr = iStr (showaddr addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats :: TiState -> IseqRep
showStats (stack, dump, heap, globals, stats)
  = iConcat [ iNewline, iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetSteps stats)
            ]

-- showHeapStats :: TiStats -> IseqRep
-- showHeapStats (a, u, f)
--   = iConcat [ iNewline, iStr "Heap allocation count = "
--             , iNum a
--             , iNewline, iStr "Heap update count     = "
--             , iNum u
--             , iNewline, iStr "Heap free count       = "
--             , iNum f
--             ]

-- showHeap :: TiHeap -> IseqRep
-- showHeap heap@(_, _, useds)
--   = iConcat
--     [ iStr "Heap  ["
--     , iIndent (iInterleave iNewline (map showHeapItem useds))
--     , iStr " ]"
--     ]
--     where
--       showHeapItem (addr, node)
--         = iConcat [ showFWAddr addr, iStr ": "
--                   , showNode node
--                   ]
      
-- showStack' :: TiHeap -> TiStack -> IseqRep
-- showStack' heap stack
--   = iConcat
--     [ iStr "Stk ["
--     , iIndent (iInterleave iNewline (map showStackItem stack))
--     , iStr " ]"
--     ]
--     where
--       showStackItem addr
--         = iConcat [ showFWAddr addr, iStr ": "
--                   , showStkNode heap (hLookup heap addr)
--                   ]

-- Test

testProg0, testProg1, testProg2 :: String
testProg0 = "main = S K K 3"
testProg1 = "main = S K K" -- wrong (not saturated)
testProg2 = "id x = x;\n\
            \main = twice twice twice id 3"

test :: Int -> String -> String
test steps = showResults . take steps . eval . compile . parse
