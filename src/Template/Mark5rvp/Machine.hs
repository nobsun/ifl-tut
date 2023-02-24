{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5rvp.Machine
    where

import Data.Bool
import Data.Char
import Data.List

import Language
import Heap
import Stack
import Utils

import Template.Mark5rvp.State
import Template.Mark5rvp.PPrint

import Debug.Trace qualified as Deb

debug :: Bool
debug = True

trace :: String -> a -> a
trace | debug     = Deb.trace
      | otherwise = const id 

traceShow :: Show a => a -> b -> b
traceShow | debug     = Deb.traceShow
          | otherwise = const id

{- * Mark 5 : Structured data -}
{- | Structure of the implementations -}

drive :: ([String] -> [String]) -> (String -> String)
drive f = unlines . f . ("" :) . lines

run :: (?sz :: Int, ?th :: Int) => String -> ([String] -> [String])
run prog inputs
    = showResults 
    $ eval
    $ setControl inputs
    $ compile 
    $ parse prog

setControl :: [String] -> TiState -> TiState
setControl ctrl state = state { control = ctrl }

{- | Compiler -}

compile :: (?sz :: Int, ?th :: Int) => CoreProgram -> TiState
compile prog = TiState
    { control = []
    , output  = []
    , stack   = initialStack1
    , dump    = initialDump
    , heap    = initialHeap1
    , globals = initialGlobals
    , stats   = initialStats
    , ruleid  = 0
    }
    where
        scDefs = prog ++ preludeDefs ++ extraPreludeDefs
        (initialHeap, initialGlobals) = buildInitialHeap scDefs
        _initialStack = singletonStack addressOfMain
        initialStack1 = singletonStack addr
        addressOfMain = aLookup initialGlobals "main" (negate 1)
        (heap1, addr1)
            | addressOfMain < 0 = (initialHeap, aLookup initialGlobals "pmain" (error "no main and no pmain"))
            | otherwise         = case aLookup initialGlobals "Cons" (error "Cons is not defined") of
                addressOfCons    -> case aLookup initialGlobals "Nil" (error "Nil is not defined") of
                    addressOfNil   -> case hAlloc initialHeap (NAp addressOfCons addressOfMain) of
                        (h, a)       -> hAlloc h (NAp a addressOfNil)
        addressOfPrint = aLookup initialGlobals "printList" (error "printList is not defined")
        (initialHeap1, addr) = hAlloc heap1 (NAp addressOfPrint addr1)

extraPreludeDefs :: CoreProgram
extraPreludeDefs = 
    [ ("False", [], EConstr 0 0)
    , ("True" , [], EConstr 1 0)
    , ("not", ["x"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                              (EVar "False"))
                         (EVar "True"))
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                   (EVar "y"))
                              (EVar "False"))
    , ("or",  ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                   (EVar "True"))
                              (EVar "y"))
    , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                   (EAp (EVar "not") (EVar "y")))
                              (EVar "y"))
    , ("MkPair", [], EConstr 0 2)
    , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p"))
                         (EVar "K"))
    , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p"))
                         (EVar "K1"))
    , ("Nil", [], EConstr 0 0)
    , ("Cons", [], EConstr 1 2)
    , ("head", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                (EVar "abort"))
                           (EVar "K"))
    , ("tail", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                (EVar "abort"))
                           (EVar "K1"))
    , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                     (EVar "stop"))
                                (EVar "printCons"))
    , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h"))
                                    (EAp (EVar "printList") (EVar "t")))
    ]

buildInitialHeap :: ( ?sz :: Int, ?th :: Int ) => [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap2, scAddrs ++ primAddrs)
    where
        (heap1, scAddrs)   = mapAccumL allocateSc hInitial scDefs
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
              | otherwise      = eval $ doAdmin $ doAdminTotalSteps $ step state

doAdmin :: TiState -> TiState
doAdmin state = bool id gc (state.heap.curAllocs > state.heap.threshold) state

step :: TiState -> TiState
step state = case map toLower $ head state.control of
    ""                -> state' { control = tail state.control }
    "c"               -> state' { control = repeat "" }
    s | all isDigit s -> state' { control = replicate (pred $ read s) "" ++ tail state.control }
      | otherwise     -> state' { control = tail state.control }
  where
        state' = dispatchNode 
                    apStep
                    scStep
                    numStep
                    indStep
                    primStep
                    dataStep
                    (error "step: NMarked node")
                    (hLookup state.heap (fst (pop state.stack)))
                  $ state

numStep :: Int -> TiState -> TiState
numStep _n state 
    | isEmptyStack state.dump = error "numStep: Number applied as a function"
    | otherwise = case restore state.stack state.dump of
        (stack1, dump1) -> setRuleId 7 $ state { stack = stack1, dump = dump1 }

apStep :: Addr -> Addr -> TiState -> TiState
apStep a1 a2 (state :: TiState) = case hLookup state.heap a2 of
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

indStep :: Addr -> TiState -> TiState
indStep addr state = setRuleId 4 $ state { stack = push addr (discard 1 state.stack) }

primStep :: Name -> Primitive -> TiState -> TiState
primStep _name prim = prim

dataStep :: Tag -> [Addr] -> TiState -> TiState
dataStep _tag _contents state = state { stack = stack1, dump = dump1 }
    where
        (stack1, dump1) = restore state.stack state.dump

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
instantiateVar heap env name
    = (heap, aLookup env name (error ("instantiateVar: Undefined name " ++ show name)))

instantiateNum :: TiHeap -> Assoc Name Addr -> Int -> (TiHeap, Addr)
instantiateNum heap _env num = hAlloc heap (NNum num)

instantiateConstr :: TiHeap -> Assoc Name Addr -> Tag -> Arity -> (TiHeap, Addr)
instantiateConstr heap _env tag arity = hAlloc heap (NPrim "Constr" (primConstr tag arity))

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
instUpdEConstr updAddr heap _env tag arity
    = hUpdate heap updAddr (NPrim "Constr" (primConstr tag arity))

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
        instantiateRhs heap' (name, rhs) = (heap'', (name, addr))
            where
                (heap'', addr) = instantiate rhs heap' rhsEnv

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

test :: (?sz :: Int, ?th :: Int) => String -> IO ()
test = interact . drive . run 

-- Gabage Collector (Pointer reversal)
gc :: TiState -> TiState
gc state = case markFromStack state.heap state.stack of
    (hp1, stack1) -> case markFromDump hp1 state.dump of
        (hp2, _dump1)  -> case markFromGlobals hp2 state.globals of
            (hp3, globals1) -> state { stack = stack1, heap = scanHeap hp3, globals = globals1, stats = incGcCount state.stats }

findStackRoots :: TiStack -> [Addr]
findStackRoots stack = stack.stkItems

findDumpRoots :: TiDump -> [Addr]
findDumpRoots _dump = []

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots globals = aRange globals

findRoots :: TiState -> [Addr]
findRoots state = concat
    [ findStackRoots state.stack
    , findDumpRoots state.dump
    , findGlobalRoots state.globals
    ]

markFromStack :: TiHeap -> TiStack -> (TiHeap, TiStack)
markFromStack hp stk = case mapAccumL markFrom hp stk.stkItems of
    (hp', stk') -> (hp', stk { stkItems = stk'})

markFromDump :: TiHeap -> TiDump -> (TiHeap, TiDump)
markFromDump hp dump = (hp, dump)

markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
markFromGlobals hp env = mapAccumL markFrom_ hp env
    where
        markFrom_ heap (name, addr) = case markFrom heap addr of
            (heap', addr') -> (heap', (name, addr'))

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = mark (GcState { forward = addr, backward = hNull, tiheap = heap })

mark :: GcState -> (TiHeap, Addr)
mark gcstate = case hLookup gcstate.tiheap gcstate.forward of
    node -> case node of
        NAp a1 a2 -> mark 
            (gcstate { forward  = a1
                     , backward = gcstate.forward
                     , tiheap   = hUpdate gcstate.tiheap gcstate.forward (NMarked (Visits 1) (NAp gcstate.backward a2))
                     })
        NSupercomb _n _as _e -> mark 
            (gcstate { tiheap = hUpdate gcstate.tiheap gcstate.forward (NMarked Done node)})
        NNum _num -> mark (gcstate { tiheap = hUpdate gcstate.tiheap gcstate.forward (NMarked Done node)})
        NInd a -> mark (gcstate { forward = a })
        NPrim _n _p -> mark (gcstate { tiheap = hUpdate gcstate.tiheap gcstate.forward (NMarked Done node)})
        NData t as -> case mapAccumL markFrom gcstate.tiheap as of
            (heap, as1) -> mark (gcstate { tiheap = hUpdate heap gcstate.forward (NMarked Done (NData t as1))})
        NMarked m _node -> case m of
            Done -> case gcstate.backward of
                b | b == hNull -> (gcstate.tiheap, gcstate.forward)
                  | otherwise  -> case hLookup gcstate.tiheap b of
                      NMarked (Visits 1) (NAp b' a2) -> mark
                        (gcstate { forward = a2
                                 , tiheap  = hUpdate gcstate.tiheap gcstate.backward (NMarked (Visits 2) (NAp gcstate.forward b'))
                                 })
                      NMarked (Visits 2) (NAp a1 b') -> mark
                        (gcstate { forward  = gcstate.backward
                                 , backward = b'
                                 , tiheap   = hUpdate gcstate.tiheap gcstate.backward (NMarked Done (NAp a1 gcstate.forward))
                                 })
                      _ -> error "mark: too many visited node"
            _ -> error "mark: invalid marked node"

--         (\ markstate node -> case markstate of
--             Done -> if gcstate.backward == hNull
--                     then (gcstate.tiheap, gcstate.forward)
--                     else case hLookup gcstate.tiheap gcstate.backward of
--                         NMarked (Visits 1) (NAp b a)
--                             -> mark (gcstate { forward = a
--                                              , tiheap = hUpdate gcstate.tiheap b (NMarked (Visits 2) (NAp gcstate.forward b))
--                                              })
--                         NMarked (Visits 2) (NAp a b)
--                             -> mark (gcstate { forward  = gcstate.backward
--                                              , backward = b
--                                              , tiheap = hUpdate gcstate.tiheap gcstate.backward (NMarked Done (NAp a gcstate.forward))
--                                              })
--                         _   -> undefined
--             _    -> undefined)
--         node
--         where
--             node = hLookup gcstate.tiheap gcstate.forward
        
{-
markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = case hLookup heap addr of
    node -> dispatchNode 
            (\ addr1 addr2 -> case markFrom heap addr1 of
                (heap1, addr1') -> case markFrom heap1 addr2 of
                    (heap2, addr2')  -> (hUpdate heap2 addr (NMarked undefined (NAp addr1' addr2')), addr))
            (\ _ _ _       -> (hUpdate heap addr (NMarked undefined node), addr)) -- NSupercomb
            (\ _           -> (hUpdate heap addr (NMarked undefined node), addr)) -- NNum
            (\ addr1       -> markFrom heap addr1)                      -- NInd
            (\ _ _         -> (hUpdate heap addr (NMarked undefined node), addr)) -- NPrim
            (\ tag as      -> case mapAccumL markFrom heap as of
                (heap', as')    -> (hUpdate heap' addr (NMarked undefined (NData tag as')), addr))
                                                                        -- NData
            (\ _ _         -> (heap, addr))                             -- NMarked
            node
-}
scanHeap :: TiHeap -> TiHeap
scanHeap heap =foldl phi heap heap.assocs
    where
        phi h (a, node) = case node of
            NMarked _ node1 -> hUpdate h a node1
            _               -> hFree h a
