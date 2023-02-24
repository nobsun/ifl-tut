{- LANGUAGE BangPatterns -}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5cp.Machine
    where

import Data.Bool
import Data.Char
import Data.List

import Language
import Heap
import Stack
import Iseq
import Utils

import Template.Mark5cp.State
import Template.Mark5cp.PPrint

import Debug.Trace qualified as Deb

debug :: Bool
debug = False

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
        -- initialStack = singletonStack addressOfMain
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

eval :: (?sz :: Int, ?th :: Int) => TiState -> [TiState]
eval state = state : rests
    where
        rests | tiFinal state = []
              | otherwise      = eval $ doAdmin $ doAdminTotalSteps $ step state

doAdmin :: (?sz :: Int, ?th :: Int) => TiState -> TiState
doAdmin state = bool id (trace ("GC@"++show state.stats.totalSteps) gc) (state.heap.curAllocs > state.heap.threshold) state

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
                    (error "step: NForward node")
                    (hLookup state.heap (fst (pop state.stack)))
                  $ state

numStep :: Int -> TiState -> TiState
numStep _n state 
    | isEmptyStack state.dump = error "numStep: Number applied as a function"
    | otherwise = case popAndRestore state.stack state.dump of
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

indStep :: Addr -> TiState -> TiState
indStep addr state = setRuleId 4 $ state { stack = push addr (discard 1 state.stack) }

primStep :: Name -> Primitive -> TiState -> TiState
primStep _name prim = prim

dataStep :: Tag -> [Addr] -> TiState -> TiState
dataStep _tag _contents state = state { stack = stack1, dump = dump1 }
    where
        (stack1, dump1) = popAndRestore state.stack state.dump

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
            = (heap1', (name, addr))
            where
                (heap1', addr) = instantiate rhs heap'' rhsEnv

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

test :: (?sz :: Int, ?th :: Int) => String -> IO ()
test = interact . drive . run 

-- Gabage Collector (Mark-scan)

gc :: (?sz :: Int, ?th :: Int) => TiState -> TiState
gc state = case evacuateStack state.heap hInitial state.stack of
    ((from1, to1), stack1) -> case evacuateDump from1 to1 state.dump of
        ((from2, to2), dump1)  -> case evacuateGlobals from2 to2 state.globals of
            ((from3, to3), globals1) -> case scavenge from3 to3 of
                to4 ->  trace (gcPrint state.heap from1 to1 from3 to3 to4) $ 
                        state { stack = stack1
                              , dump = dump1
                              , heap = hIncThreshold to4
                              , globals = globals1
                              , stats = incGcCount state.stats
                              }
    where
        gcPrint h0 f1 t1 f3 t3 t4 =  iDisplay $ iConcat [ iNewline
                                                        , iStr "vvvvvvvvvvvvvvvvvvvv"
                                                        , iNewline
                                                        , iStr "before:"
                                                        , iNewline
                                                        , showHeap h0
                                                        , iNewline
                                                        , iStr "evacuated: from1"
                                                        , iNewline
                                                        , showHeap f1
                                                        , iNewline
                                                        , iStr "evacuated: to1"
                                                        , iNewline
                                                        , showHeap t1
                                                        , iNewline 
                                                        , iStr "evacuated: from3"
                                                        , iNewline
                                                        , showHeap f3
                                                        , iNewline
                                                        , iStr "evacuated: to3"
                                                        , iNewline
                                                        , showHeap t3
                                                        , iNewline 
                                                        , iStr "scavenged: after"
                                                        , iNewline
                                                        , showHeap t4
                                                        , iNewline
                                                        , iStr "^^^^^^^^^^^^^^^^^^^^"
                                                        , iNewline
                                                        ]
        -- printHeap heap = iDisplay $ iConcat [ iStr "scavenged: to"
        --                                     , iNewline
        --                                     , showHeap heap
        --                                     , iNewline
        --                                     ]
        hIncThreshold h = if debug  then h { threshold = 2 * h.threshold } else h

evacuateStack :: TiHeap -> TiHeap -> TiStack -> ((TiHeap, TiHeap), TiStack)
evacuateStack from to stack = case mapAccumL evacuateFrom (from, to) stack.stkItems of
    (heaps', addrs') -> (heaps', stack { stkItems = addrs'})

evacuateDump :: TiHeap -> TiHeap -> TiDump -> ((TiHeap, TiHeap), TiDump)
evacuateDump from to dump = ((from, to), dump)

evacuateGlobals :: TiHeap -> TiHeap -> TiGlobals -> ((TiHeap, TiHeap), TiGlobals)
evacuateGlobals from to globals = case unzip globals of
    (names, addrs) -> case mapAccumL evacuateFrom (from, to) addrs of
        ((from1, to1), addrs1) -> ((from1, to1), zip names addrs1)  

evacuateFrom :: (TiHeap, TiHeap) -> Addr -> ((TiHeap, TiHeap), Addr)
evacuateFrom (from, to) a = case trace "eva:01" hLookup from a of
    node -> case node of
        NAp b c -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> case evacuateFrom (from1, to1) b of
                    ((from2, to2), _) -> case evacuateFrom (from2, to2) c of
                        ((from3, to3), _) -> ((from3, to3), a')
        NInd b -> case evacuateFrom (from, to) b of
            ((from1, to1), b') -> ((hUpdate from1 a (NForward b'), to1), b')
        NData _name args -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> case mapAccumL evacuateFrom (from1, to1) args of
                    ((from2, to2), _) -> ((from2, to2), a')
        NForward a' -> ((from, to), a')
        _ -> case hAlloc to node of
            (to1, a') -> case hUpdate from a (NForward a') of
                from1     -> ((from1, to1), a')

scavenge :: TiHeap -> TiHeap -> TiHeap
scavenge from to = foldl phi to to.assocs
    where
        phi t (a', n) = case n of
            NAp b c -> case trace "sca:01" hLookup from b of
                NForward b' -> case trace "sca:02" hLookup from c of
                    NForward c'   -> hUpdate t a' (NAp b' c')
                    _ -> error "scavenge: not NForward node"
                _ -> error "scavenge: not NForward node"
            NInd _  -> error "scavenge: NInd"
            NData name args -> hUpdate t a' (NData name (map (unNF . trace "sca:03" hLookup from) args))
            _ -> t
        unNF node = case node of
            NForward fw -> fw
            _           -> error "scavenge: not NForward"

            
