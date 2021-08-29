{-# LANGUAGE TypeApplications #-}
module Template.Mark1 where

import Data.List

import Iseq
import Utils hiding (showAddr)
import Language 

runProg :: String -> String
runProg = showResults . eval . compile . parse

compile :: CoreProgram -> TiState
-- compile = undefined

eval :: TiState -> [TiState]
-- eval = undefined

showResults :: [TiState] -> String

{- | Data types
-}

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump deriving Show
initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap  = Heap Node

data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    deriving Show

type TiGlobals = ASSOC Name Addr

type TiStats = Int
tiStatInitial :: TiStats
tiStatInitial = 0
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = succ s
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statsFun (stack, dump, heap, globals, stats)
    = (stack, dump, heap, globals, statsFun stats)

{- | コンパイラ
-}
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        initial_stack = [address_of_main]
        address_of_main = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: [CoreScDefn]
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccumL allocateSc hInitial sc_defs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
    = (heap', (name, addr))
        where
            (heap', addr) = hAlloc heap (NSupercomb name args body)

{- | Evaluator 
-}
eval state = state : rest_states
    where
        rest_states
            | tiFinal state = []
            | otherwise     = eval next_state
        next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats)
    = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats)
    = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats)
    = dispatch (hLookup heap (head stack))
        where
            dispatch (NNum n)                  = numStep state n
            dispatch (NAp a1 a2)               = apStep  state a1 a2
            dispatch (NSupercomb sc args body) = scStep  state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
    = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
    = (new_stack, dump, new_heap, globals, stats)
    where
        new_stack = result_addr : (drop (length arg_names + 1) stack)
        (new_heap, result_addr) = instantiate body heap env
        env = arg_bindings ++ globals
        arg_bindings = zip arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map get_arg stack
    where
        get_arg addr = arg
            where
                (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr
            -> TiHeap
            -> ASSOC Name Addr
            -> (TiHeap, Addr)
instantiate e heap env = case e of
    ENum n    -> hAlloc heap (NNum n)
    EAp e1 e2 -> hAlloc heap2 (NAp a1 a2)
        where
            (heap1, a1) = instantiate e1 heap env
            (heap2, a2) = instantiate e2 heap env
    EVar v    -> (heap, aLookup env v (error ("Undefined name " ++ v)))
    EConstr tag arity -> instantiateConstr tag arity heap env
    ELet isrec defs body -> instantiateLet isrec defs body heap env
    ECase e alts -> error "Can't instantiate case exprs"

instantiateConstr :: Tag -> Arity -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env
    = error "Can't instatiate constructors yet"
instantiateLet :: IsRec -> ASSOC Name CoreExpr -> CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiateLet isrec defs body heap env
    = error "Can't instantiate let(rec)s yet"

showResults states
    = iDisplay @IseqRep (iConcat [ iLayn (map showState states), showStats (last states)])

showState :: Iseq iseq => TiState -> iseq
showState (stack, dump, heap, globals, stats)
    = iConcat [ showStack heap stack, iNewline]

showStack :: Iseq iseq => TiHeap -> TiStack -> iseq
showStack heap stack
    = iConcat 
        [ iStr "Stk ["
        , iIndent (iInterleave iNewline (map show_stack_item stack))
        , iStr "]"
        ]
    where
        show_stack_item addr
            = iConcat [ showFWAddr addr, iStr ": "
                      , showStkNode heap (hLookup heap addr)
                      ]

showStkNode :: Iseq iseq => TiHeap -> Node -> iseq
showStkNode heap (NAp fun_addr arg_addr)
    = iConcat
        [ iStr "NAp ", showFWAddr fun_addr
        , iStr " ", showFWAddr arg_addr, iStr " ("
        , showNode (hLookup heap arg_addr), iStr ")"
        ]
showStkNode heap node = showNode node

showNode :: Iseq iseq => Node -> iseq
showNode node = case node of
    NAp a1 a2 -> iConcat [ iStr "NAp ", showAddr a1
                         , iStr " ",    showAddr a2
                         ]
    NSupercomb name args body -> iStr ("NSupercomb " ++ name)
    NNum n -> iStr "NNum " `iAppend` iNum n

showAddr :: Iseq iseq => Addr -> iseq
showAddr addr = iStr (show addr)

showFWAddr :: Iseq iseq => Addr -> iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
    where
        str = show addr

showStats :: Iseq iseq => TiState -> iseq
showStats (stack, dump, heap, globals, stats)
    = iConcat [ iNewline, iNewline, iStr "Total number of steps = "
              , iNum (tiStatGetSteps stats)
              ]

ex204 :: String
ex204 = "main = S K K 3"
