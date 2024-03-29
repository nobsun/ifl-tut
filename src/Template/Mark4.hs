{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark4
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

{- * Mark 4 : Adding arithmetic -}

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

type TiDump    = Stack TiStack
initialDump :: TiDump
initialDump = emptyStack

type TiHeap    = Heap Node

data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    | NInd Addr
    | NPrim Name Primitive
    deriving Show

dispatchNode :: (Addr -> Addr -> a)               -- ^ NAp
             -> (Name -> [Name] -> CoreExpr -> a) -- ^ NSupercomb
             -> (Int -> a)                        -- ^ NInt
             -> (Addr -> a)                       -- ^ NInd
             -> (Name -> Primitive -> a)          -- ^ NPrim
             -> Node -> a
dispatchNode nap nsupercomb nnum nind nprim node = case node of
    NAp a b                -> nap a b
    NSupercomb f args body -> nsupercomb f args body
    NNum n                 -> nnum n
    NInd a                 -> nind a
    NPrim name prim        -> nprim name prim

data Primitive
    = Neg
    | Add
    | Sub
    | Mul
    | Div
    deriving Show

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add), ("-", Sub)
             , ("*", Mul), ("/", Div)
             ]

type TiGlobals = Assoc Name Addr

data TiStats 
    = TiStats
    { totalSteps :: Int
    , scSteps    :: Int
    , primSteps  :: Int
    }
    deriving Show

initialStats :: TiStats
initialStats = TiStats { totalSteps = 0, scSteps = 0, primSteps = 0 }

incTotalSteps, incScSteps, incPrimSteps :: TiStats -> TiStats
incTotalSteps stats = stats { totalSteps = succ stats.totalSteps }
incScSteps    stats = stats { scSteps    = succ stats.scSteps }
incPrimSteps  stats = stats { primSteps  = succ stats.primSteps }

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

defaultThreshold :: Int
defaultThreshold = 50

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap2, scAddrs ++ primAddrs)
    where
        (heap1, scAddrs)   = let { ?sz = defaultHeapSize; ?th = defaultThreshold }
                             in mapAccumL allocateSc hInitial scDefs
        (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
    (name, args, body) -> (heap', (name, addr))
        where
            (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap1, (name, addr))
    where
        (heap1, addr) = hAlloc heap (NPrim name prim)

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
                                  && isEmptyStack state.dump
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
step state = dispatchNode 
             apStep
             scStep
             numStep
             indStep
             primStep
             (hLookup state.heap (fst (pop state.stack)))
           $ state

numStep :: Int -> TiState -> TiState
numStep _n state 
    | isEmptyStack state.dump = error "numStep: Number applied as a function"
    | otherwise = case pop state.dump of
        (stack1, dump1) -> setRuleId 7 $ state { stack = stack1, dump = dump1 }

apStep :: Addr -> Addr -> TiState -> TiState
apStep a1 a2 state = case hLookup state.heap a2 of
    NInd a3 -> setRuleId 8 $ state { heap = hUpdate state.heap a (NAp a1 a3) }
    _       -> setRuleId 1 $ state { stack = push a1 state.stack }
    where
        (a,_) = pop state.stack

scStep :: Name -> [Name] -> CoreExpr -> TiState -> TiState
scStep _name args body state
    | state.stack.curDepth < succ argsLen
        = error "scStep: too few arguments given"
    | otherwise
        = doAdminScSteps $ setRuleId 3 $ state { stack = stack1, heap = heap1 }
    where
        argsLen  = length args
        stack1   = discard argsLen state.stack
        (root,_) = pop stack1
        heap1    = instantiateAndUpdate body root state.heap (bindings ++ state.globals)
        bindings = zip args (getargs state.heap state.stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case pop stack of
    (_sc, stack') -> map getarg stack'.stkItems
        where
            getarg addr = arg
                where
                    arg = case hLookup heap addr of
                        NAp _fun a -> a
                        _          -> error "getargs: not NAp node"

indStep :: Addr -> TiState -> TiState
indStep addr state = setRuleId 4 $ state { stack = push addr (discard 1 state.stack) }

primStep :: Name -> Primitive -> TiState -> TiState
primStep _name prim = case prim of
    Neg -> primNeg
    Add -> primArith (+)
    Sub -> primArith (-)
    Mul -> primArith (*)
    Div -> primArith div

primNeg :: TiState -> TiState
primNeg state
    | length args /= 1         = error "primNeg: wrong number of args"
    | not (isDataNode argNode) = setRuleId 9
                               $ state { stack = singletonStack argAddr
                                       , dump  = push stack1 state.dump 
                                       }
    | otherwise                = doAdminPrimSteps $ setRuleId 5 
                               $ state { stack = stack1, heap = heap1 }
    where
        args      = getargs state.heap state.stack
        argAddr   = head args
        argNode   = hLookup state.heap argAddr
        argValue = case argNode of
            NNum n -> n
            _ -> error "primNeg: NaN operand"
        (_, stack1) = pop state.stack
        (root, _)   = pop stack1
        heap1 = hUpdate state.heap root (NNum (negate argValue))

primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith op state
    | length args /= 2 = error "primArith: wrong number of args"
    | not (isDataNode argNode1) = state { stack = singletonStack argAddr1
                                        , dump  = push stack1 state.dump 
                                        }
    | not (isDataNode argNode2) = state { stack = singletonStack argAddr2
                                        , dump  = push stack1 state.dump
                                        }
    | otherwise                 = doAdminPrimSteps $ setRuleId 17
                                $ state { stack = stack1, heap = heap1 }
    where
        args = getargs state.heap state.stack
        argAddr1 = head args
        argAddr2 = head $ tail args
        argNode1 = head argNodes
        argNode2 = head $ tail argNodes
        argNodes = map (hLookup state.heap) args
        argVal1 = case argNode1 of
            NNum n -> n
            _ -> error "primArith: NaN operand"
        argVal2 = case argNode2 of
            NNum n -> n
            _ -> error "primArith: NaN operand"
        stack1 = discard 2 state.stack
        (root, _) = pop stack1
        heap1 = hUpdate state.heap root (NNum (op argVal1 argVal2))
        
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
instantiateNum heap _env num = hAlloc heap (NNum num)

instantiateConstr :: TiHeap -> Assoc Name Addr -> Tag -> Arity -> (TiHeap, Addr)
instantiateConstr _heap _env _tag _arity = error "Cannot instantiate constructor yet"

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
        instantiateRhs heap'' (name, rhs)
            = (heap1, (name, addr))
            where
                (heap1, addr) = instantiate rhs heap'' rhsEnv

instantiateCase :: TiHeap -> Assoc Name Addr -> CoreExpr -> [CoreAlt] -> (TiHeap, Addr)
instantiateCase _heap _env _expr _alters = error "Cannot instatiate case"

instantiateLam :: TiHeap -> Assoc Name Addr -> [Name] -> CoreExpr -> (TiHeap, Addr)
instantiateLam _heap _env _vars _body = error "Cannot instatiate lambda"

instantiateAndUpdate :: CoreExpr
                     -> Addr
                     -> TiHeap
                     -> Assoc Name Addr
                     -> TiHeap
instantiateAndUpdate expr updAddr heap env = dispatchCoreExpr
    (instUpdEVar updAddr heap env)
    (instUpdENum updAddr heap env)
    (instUpdEConstr updAddr heap env)
    (instUpdEAp updAddr heap env)
    (instUpdELet updAddr heap env)
    (instUpdECase updAddr heap env)
    (instUpdELam updAddr heap env)
    expr

instUpdEVar :: Addr
            -> TiHeap
            -> Assoc Name Addr
            -> Name
            -> TiHeap
instUpdEVar updAddr heap env v = hUpdate heap updAddr (NInd varAddr)
    where
        varAddr = aLookup env v (error ("undefined name " ++ show v))

instUpdENum :: Addr
            -> TiHeap
            -> Assoc Name Addr
            -> Int
            -> TiHeap
instUpdENum updAddr heap _env n = hUpdate heap updAddr (NNum n)

instUpdEConstr :: Addr
               -> TiHeap
               -> Assoc Name Addr
               -> Tag
               -> Arity
               -> TiHeap
instUpdEConstr _updAddr _heap _env _tag _arity = error "not implemented yet"

instUpdEAp :: Addr
           -> TiHeap
           -> Assoc Name Addr
           -> CoreExpr
           -> CoreExpr
           -> TiHeap
instUpdEAp updAddr heap env e1 e2 = hUpdate heap2 updAddr (NAp a1 a2)
    where
        (heap1, a1) = instantiate e1 heap  env
        (heap2, a2) = instantiate e2 heap1 env

instUpdELet :: Addr
            -> TiHeap
            -> Assoc Name Addr
            -> IsRec
            -> Assoc Name CoreExpr
            -> CoreExpr
            -> TiHeap
instUpdELet updAddr heap env isrec defs body = instantiateAndUpdate body updAddr heap1 env1
    where
        (heap1, extraBindings) = mapAccumL instantiateRhs heap defs
        env1 = extraBindings ++ env
        rhsEnv | isrec     = env1
               | otherwise = env
        instantiateRhs heap' (name, rhs) = (heap1', (name, addr))
            where
                (heap1', addr) = instantiate rhs heap' rhsEnv

instUpdECase :: Addr
             -> TiHeap
             -> Assoc Name Addr
             -> CoreExpr
             -> [CoreAlt]
             -> TiHeap
instUpdECase _updAddr _heap _env _expr _alts = error "not implemented"

instUpdELam :: Addr
            -> TiHeap
            -> Assoc Name Addr
            -> [Name]
            -> CoreExpr
            -> TiHeap
instUpdELam _updAddr _heap _env _vars _body = error "not implemented"

{- | Formatting Results -}

showResults :: [TiState] -> String
showResults = concatMap iDisplay . iLayn' 0 . mapoid (showState, showStats)

mapoid :: (a -> b, a -> b) -> [a] -> [b]
mapoid (f, g) (x:xs) = case xs of
    [] -> f x : [g x]
    _  -> f x : mapoid (f,g) xs
mapoid _ _ = error "mapoid: empty list"

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
    (\ name _args _body -> iStr ("NSupercomb " ++ name))
    (\ n -> iStr "NNum " `iAppend` iNum n)
    (\ a -> iStr "NInd " `iAppend` showAddr a)
    (\ name _ -> iStr ("NPrim " ++ name))
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
    (\ _ -> showNode node)
    (\ _ _ -> showNode node)
    node

showRuleId :: TiRuleId -> IseqRep
showRuleId rid = iStr ("Rule " ++ show (2 :: Int, rid)) 

showStats :: TiState -> IseqRep
showStats state = iConcat
    [ iNewline, iStr "Total number of steps = "
    , iNum state.stats.totalSteps
    , iNewline, iStr "             Sc steps = "
    , iNum state.stats.scSteps
    , iNewline, iStr "           Prim steps = "
    , iNum state.stats.primSteps
    , iNewline, iStr "     Allocation count = "
    , iNum state.heap.maxAllocs
    , iNewline, iStr "   Max depth of stack = "
    , iNum state.stack.maxDepth
    ]

{- | Testing -}

test :: String -> IO ()
test = putStr . run

