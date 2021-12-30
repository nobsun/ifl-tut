{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark2
    where

import Data.List

import Language
import Heap
import Stack
import Iseq
import Utils

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id 

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

{- * Mark 1 : A minimal template instantiation graph reducer -}

{- | Types -}

data TiState
    = TiState
    { stack   :: TiStack
    , dump    :: TiDump
    , heap    :: TiHeap
    , globals :: TiGlobals
    , stats   :: TiStats
    , ruleid  :: TiRuleId
    }

type TiStack   = Stack Addr

type TiDump    = DummyTiDump
data DummyTiDump = DummyTiDump deriving Show
initialDump :: TiDump
initialDump = DummyTiDump

type TiHeap    = Heap Node

data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    deriving Show

dispatchNode :: (Addr -> Addr -> a)               -- ^ NAp
             -> (Name -> [Name] -> CoreExpr -> a) -- ^ NSupercomb
             -> (Int -> a)                        -- ^ NInt
             -> Node -> a
dispatchNode nap nsupercomb nnum node = case node of
    NAp a b                -> nap a b
    NSupercomb f args body -> nsupercomb f args body
    NNum n                 -> nnum n

type TiGlobals = Assoc Name Addr

data TiStats 
    = TiStats
    { totalSteps :: Int
    , scSteps    :: Int
    , primSteps  :: Int
    , deltaSteps :: Int
    }
    deriving Show

initialStats :: TiStats
initialStats = TiStats { totalSteps = 0, scSteps = 0, primSteps = 0, deltaSteps = 0 }

incTotalSteps, incScSteps, incPrimSteps :: TiStats -> TiStats
incTotalSteps stats = stats { totalSteps = succ stats.totalSteps }
incScSteps    stats = stats { scSteps    = succ stats.scSteps }
incPrimSteps  stats = stats { primSteps  = succ stats.primSteps }
incDeltaSteps stats = stats { deltaSteps = succ stats.deltaSteps }

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = state { stats = f state.stats }

type TiRuleId = Int

setRuleId :: TiRuleId -> TiState -> TiState
setRuleId r state = state { ruleid = r }

{- | Structure of the implementations -}

run :: String -> String
run = showResults . eval . compile . parse

{- | Compiler -}

compile :: CoreProgram -> TiState
compile prog = TiState
    { stack   = initialStack
    , dump    = initialDump
    , heap    = initialHeap
    , globals = initialGlobals
    , stats   = initialStats
    , ruleid  = 0
    }
    where
        scDefs = prog ++ preludeDefs ++ extraPreludeDefs
        (initialHeap, initialGlobals) = buildInitialHeap scDefs
        initialStack = singletonStack addressOfMain
        addressOfMain = aLookup initialGlobals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

defaultHeapSize :: Int
defaultHeapSize = 1024

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = let ?sz = defaultHeapSize in
    mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
    (name, args, body) -> (heap', (name, addr))
        where
            (heap', addr) = hAlloc heap (NSupercomb name args body)

{- | Evaluator -}

eval :: TiState -> [TiState]
eval state = state : rests
    where
        rests | tiFinal state = []
              | otherwise      = eval $ doAdminTotalSteps $ step state

tiFinal :: TiState -> Bool
tiFinal state
    | isEmptyStack state.stack     = error "tiFinal: empty stack"
    | isSingletonStack state.stack = isDataNode (hLookup state.heap soleAddr)
    | otherwise                    = False
    where
        (soleAddr, _) = pop state.stack

isDataNode :: Node -> Bool
isDataNode node = case node of
    NNum _ -> True
    _      -> False

doAdminTotalSteps :: TiState -> TiState
doAdminTotalSteps = applyToStats incTotalSteps

doAdminScSteps :: TiState -> TiState
doAdminScSteps = applyToStats incScSteps

doAdminPrimSteps :: TiState -> TiState
doAdminPrimSteps = applyToStats incPrimSteps

step :: TiState -> TiState
step state = dispatchNode apStep scStep numStep (hLookup state.heap (fst (pop state.stack)))
           $ state

numStep :: Int -> TiState -> TiState
numStep n = error "numStep: Number applied as a function"

apStep :: Addr -> Addr -> TiState -> TiState
apStep a b state = setRuleId 1 $ state { stack = push a (state.stack :: TiStack) }

scStep :: Name -> [Name] -> CoreExpr -> TiState -> TiState
scStep name args body state
    | state.stack.curDepth < n' 
        = error "scStep: too few arguments given"
    | otherwise
        = doAdminScSteps $ setRuleId 2 $ state { stack = stack', heap = heap' }
    where
        stack' = push resultAddr (discard n' state.stack)
        (heap', resultAddr) = instantiate body state.heap env
        env = argBindings ++ state.globals
        argBindings = zip args (getargs state.heap state.stack)
        n' = succ (length args)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case pop stack of
    (sc, stack') -> map getarg stack'.stkItems
        where
            getarg addr = arg
                where
                    NAp fun arg = hLookup heap addr

{- | Instantiation -}

instantiate :: CoreExpr
            -> TiHeap
            -> Assoc Name Addr
            -> (TiHeap, Addr)
instantiate expr heap env = dispatchCoreExpr
    (instantiateVar heap env)
    (instantiateNum heap env)
    (instantiateConstr heap env)
    (instantiateAp heap env)
    (instantiateLet heap env)
    (instantiateCase heap env)
    (instantiateLam heap env)
    expr

instantiateVar :: TiHeap -> Assoc Name Addr -> Name -> (TiHeap, Addr)
instantiateVar heap env name = (heap, aLookup env name (error ("instantiateVar: Undefined name " ++ show name)))

instantiateNum :: TiHeap -> Assoc Name Addr -> Int -> (TiHeap, Addr)
instantiateNum heap env num = hAlloc heap (NNum num)

instantiateConstr :: TiHeap -> Assoc Name Addr -> Tag -> Arity -> (TiHeap, Addr)
instantiateConstr heap env tag arity = error "Cannot instantiate constructor yet"

instantiateAp :: TiHeap -> Assoc Name Addr -> CoreExpr -> CoreExpr -> (TiHeap, Addr)
instantiateAp heap env a b = hAlloc heap2 (NAp a1 a2)
    where
        (heap1, a1) = instantiate a heap  env
        (heap2, a2) = instantiate b heap1 env

instantiateLet :: TiHeap -> Assoc Name Addr -> IsRec -> Assoc Name CoreExpr -> CoreExpr -> (TiHeap, Addr)
instantiateLet heap env isrec defs body = instantiate body heap' env'
    where
        (heap', extraBindings) = mapAccumL instantiateRhs heap defs
        env' = extraBindings ++ env
        rhsEnv | isrec     = env'
               | otherwise = env
        instantiateRhs heap (name, rhs)
            = (heap1, (name, addr))
            where
                (heap1, addr) = instantiate rhs heap rhsEnv

instantiateCase :: TiHeap -> Assoc Name Addr -> CoreExpr -> [CoreAlter] -> (TiHeap, Addr)
instantiateCase heap env expr alters = error "Cannot instatiate case"

instantiateLam :: TiHeap -> Assoc Name Addr -> [Name] -> CoreExpr -> (TiHeap, Addr)
instantiateLam heap env vars body = error "Cannot instatiate lambda"

{- | Formatting Results -}

showResults :: [TiState] -> String
showResults = concatMap iDisplay . iLayn' 0 . mapoid (showState, showStats)

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs

showState :: TiState -> IseqRep
showState state = iConcat
    [ showHeap state.heap, iNewline
    , showStack state.heap state.stack, iNewline
    , showRuleId state.ruleid, iNewline
    ]

showHeap :: TiHeap -> IseqRep
showHeap heap = iConcat
    [ iStr "Heap ["
    , iIndent (iInterleave iNewline (map showHeapItem heap.assocs))
    , iStr " ]"
    ]

showHeapItem :: (Addr, Node) -> IseqRep
showHeapItem (addr, node) = iConcat
            [ showFWAddr addr, iStr ": "
            , showNode node
            ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr ('#' : show addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (rjustify 4 (show addr))

showNode :: Node -> IseqRep
showNode node = dispatchNode
    (\ a1 a2 -> iConcat [ iStr "NAp ", showAddr a1, iStr " ", showAddr a2 ])
    (\ name args body -> iStr ("NSupercomb " ++ name))
    (\ n -> iStr "NNum " `iAppend` iNum n)
    node

showStack :: TiHeap -> TiStack -> IseqRep
showStack heap stack = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem stack.stkItems))
    , iStr " ]"
    ]
    where
        showStackItem addr = iConcat
            [ showFWAddr addr, iStr ": "
            , showStkNode heap (hLookup heap addr)
            ]

showStkNode :: TiHeap -> Node -> IseqRep
showStkNode heap node = dispatchNode
    (\ funAddr argAddr -> iConcat [ iStr "NAp ", showFWAddr funAddr
                                  , iStr " ", showFWAddr argAddr, iStr " ("
                                  , showNode (hLookup heap argAddr), iStr ")" ])
    (\ _ _ _ -> showNode node)
    (\ _ -> showNode node)
    node

showRuleId :: TiRuleId -> IseqRep
showRuleId rid = iStr ("Rule " ++ show (2, rid)) 

showStats :: TiState -> IseqRep
showStats state = iConcat
    [ iNewline, iStr "Total number of steps = "
    , iNum state.stats.totalSteps
    , iNewline, iStr "             Sc steps = "
    , iNum state.stats.scSteps
    , iNewline, iStr "           Prim steps = "
    , iNum state.stats.primSteps
    , iNewline, iStr "          Delta steps = "
    , iNum state.stats.deltaSteps
    , iNewline, iStr "     Allocation count = "
    , iNum state.heap.maxAllocs
    , iNewline, iStr "   Max depth of stack = "
    , iNum state.stack.maxDepth
    ]

{- | Testing -}

test :: String -> IO ()
test = putStr . run

