{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5a.Machine
    where

import Data.List

import Language
import Heap
import Stack
import Iseq
import Utils

import Template.Mark5a.State
import Template.Mark5a.PPrint

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

run :: String -> String
run = showResults . eval . compile . parse

{- | Compiler -}

compile :: CoreProgram -> TiState
compile prog = TiState
    { output  = []
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
        initialStack = singletonStack addressOfMain
        initialStack1 = singletonStack addr
        addressOfMain = aLookup initialGlobals "main" (error "main is not defined")
        addressOfPrint = aLookup initialGlobals "printList" (error "printList is not defined")
        (initialHeap1, addr) = hAlloc initialHeap (NAp addressOfPrint addressOfMain)

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

defaultHeapSize :: Int
defaultHeapSize = 1024

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap2, scAddrs ++ primAddrs)
    where
        (heap1, scAddrs)   = let ?sz = defaultHeapSize
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

step :: TiState -> TiState
step state = dispatchNode 
             apStep
             scStep
             numStep
             indStep
             primStep
             dataStep
             (hLookup state.heap (fst (pop state.stack)))
           $ state

numStep :: Int -> TiState -> TiState
numStep n state 
    | isEmptyStack state.dump = error "numStep: Number applied as a function"
    | otherwise = case restore state.stack state.dump of
        (stack1, dump1) -> setRuleId 7 $ state { stack = stack1, dump = dump1 }

apStep :: Addr -> Addr -> TiState -> TiState
apStep a1 a2 state = case hLookup state.heap a2 of
    NInd a3 -> setRuleId 8 $ state { heap = hUpdate state.heap a (NAp a1 a3) }
    _       -> setRuleId 1 $ state { stack = push a1 state.stack }
    where
        (a,_) = pop state.stack

scStep :: Name -> [Name] -> CoreExpr -> TiState -> TiState
scStep name args body state
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
primStep name prim = prim

dataStep :: Tag -> [Addr] -> TiState -> TiState
dataStep tag contents state = state { stack = stack1, dump = dump1 }
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
instantiateNum heap env num = hAlloc heap (NNum num)

instantiateConstr :: TiHeap -> Assoc Name Addr -> Tag -> Arity -> (TiHeap, Addr)
instantiateConstr heap env tag arity = hAlloc heap (NPrim "Constr" (primConstr tag arity))

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
instUpdENum updAddr heap env n = hUpdate heap updAddr (NNum n)

instUpdEConstr :: Addr
               -> TiHeap
               -> Assoc Name Addr
               -> Tag
               -> Arity
               -> TiHeap
instUpdEConstr updAddr heap env tag arity
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
        instantiateRhs heap (name, rhs) = (heap1, (name, addr))
            where
                (heap1, addr) = instantiate rhs heap rhsEnv

instUpdECase :: Addr
             -> TiHeap
             -> Assoc Name Addr
             -> CoreExpr
             -> [CoreAlter]
             -> TiHeap
instUpdECase updAddr heap env expr alts = error "not implemented"

instUpdELam :: Addr
            -> TiHeap
            -> Assoc Name Addr
            -> [Name]
            -> CoreExpr
            -> TiHeap
instUpdELam updAddr heap env vars body = error "not implemented"

{- | Testing -}

test :: String -> IO ()
test = putStr . run

