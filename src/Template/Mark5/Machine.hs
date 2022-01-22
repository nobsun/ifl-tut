{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Template.Mark5.Machine
    where

import Data.List

import Language
import Heap
import Stack
import Iseq
import Utils

import Template.Mark5.Node
import Template.Mark5.Primitive
import Template.Mark5.State
import Template.Mark5.PPrint

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

tiFinal :: TiState -> Bool
tiFinal state
    | isEmptyStack state.stack     = True
    | isSingletonStack state.stack = isDataNode (hLookup state.heap soleAddr)
                                  && isEmptyStack state.dump
    | otherwise                    = False
    where
        (soleAddr, _) = pop state.stack

isDataNode :: Node -> Bool
isDataNode node = case node of
    NNum _    -> True
    NData _ _ -> True
    _         -> False

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
             dataStep
             (hLookup state.heap (fst (pop state.stack)))
           $ state

numStep :: Int -> TiState -> TiState
numStep n state 
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

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case pop stack of
    (sc, stack') -> map getarg stack'.stkItems
        where
            getarg addr = arg
                where
                    NAp fun arg = hLookup heap addr

indStep :: Addr -> TiState -> TiState
indStep addr state = setRuleId 4 $ state { stack = push addr (discard 1 state.stack) }

primStep :: Name -> Primitive -> TiState -> TiState
primStep name prim = case prim of
    Neg -> primNeg
    Add -> primArith (+)
    Sub -> primArith (-)
    Mul -> primArith (*)
    Div -> primArith div
    Less -> primComp (<)
    LessEq -> primComp (<=)
    Greater -> primComp (>)
    GreaterEq -> primComp (>=)
    Eq -> primComp (==)
    NotEq -> primComp (/=)
    PrimConstr tag arity -> primConstr tag arity
    If  -> primIf
    PrimCasePair -> primCasePair
    PrimCaseList -> primCaseList
    Abort -> primAbort
    Stop -> primStop
    Print -> primPrint

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
        [argAddr] = args
        argNode   = hLookup state.heap argAddr
        NNum argValue = argNode
        (_, stack1) = pop state.stack
        (root, _)   = pop stack1
        heap1 = hUpdate state.heap root (NNum (negate argValue))

primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith op = primDyadic op'
    where
        op' (NNum m) (NNum n) = NNum (m `op` n)

primComp :: (Int -> Int -> Bool) -> TiState -> TiState
primComp op = primDyadic op'
    where
        op' (NNum m) (NNum n)
            | m `op` n  = NData 1 []
            | otherwise = NData 0 []

primDyadic :: (Node -> Node -> Node) -> TiState -> TiState
primDyadic op state 
    | length args /= 2 = error "primDyadic: wrong number of args"
    | not (isDataNode arg1Node) = state { stack = singletonStack arg1Addr
                                        , dump  = push stack1 state.dump 
                                        }
    | not (isDataNode arg2Node) = state { stack = singletonStack arg2Addr
                                        , dump  = push stack1 state.dump
                                        }
    | otherwise                 = doAdminPrimSteps $ setRuleId 17
                                $ state { stack = stack1, heap = heap1 }
    where
        args = getargs state.heap state.stack
        [arg1Addr, arg2Addr] = args
        [arg1Node, arg2Node] = map (hLookup state.heap) args
        stack1 = discard 2 state.stack
        (root, _) = pop stack1
        heap1 = hUpdate state.heap root (op arg1Node arg2Node)

primConstr :: Tag -> Arity -> TiState -> TiState
primConstr tag arity state
    | length args < arity = error "primConstr: wrong number of args"
    | otherwise           = setRuleId 10 $ state { stack = stack1, heap = heap1 }
    where
        args = getargs state.heap state.stack
        stack1 = discard arity state.stack
        (root,_) = pop stack1
        heap1 = hUpdate state.heap root (NData tag args)

primIf :: TiState -> TiState
primIf state
    | length args < 3 = error "primIf: wrong number of args"
    | not (isDataNode arg1Node) = setRuleId 20
                                 $ state { stack = singletonStack arg1Addr, dump = push stack1 state.dump}
    | otherwise = doAdminPrimSteps $ setRuleId 19 $ state { stack = stack1, heap = heap1}
    where
        args = getargs state.heap state.stack
        [arg1Addr, arg2Addr, arg3Addr] = take 3 args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 3 state.stack 
        (root, _) = pop stack1
        result = case arg1Node of
            NData 0 [] -> arg3Addr
            _          -> arg2Addr
        heap1 = hUpdate state.heap root (NInd result)

primCasePair :: TiState -> TiState
primCasePair state
    | length args /= 2 = error "primCasePair: wrong number of args"
    | not (isDataNode arg1Node) = state { stack = singletonStack arg1Addr
                                        , dump = push stack1 state.dump }
    | otherwise = doAdminPrimSteps $ state { stack = stack1, heap = heap1 }
    where
        args = getargs state.heap state.stack
        [arg1Addr, arg2Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 2 state.stack
        (root, _) = pop stack1
        heap1 = case arg1Node of
            NData tag [ft,sd] -> hUpdate heap2 root (NAp addr sd)
                where
                    (heap2 ,addr) = hAlloc state.heap (NAp arg2Addr ft)

primCaseList :: TiState -> TiState
primCaseList state
    | length args < 3 = error "primCaseList: wrong number of args"
    | not (isDataNode arg1Node) = state { stack = singletonStack arg1Addr
                                        , dump = push stack1 state.dump }
    | otherwise = doAdminPrimSteps $ state { stack = stack1, heap = heap1 }
    where
        args = getargs state.heap state.stack
        [arg1Addr, arg2Addr, arg3Addr] = take 3 args
        arg1Node = hLookup state.heap arg1Addr
        stack1 = discard 3 state.stack
        (root, _) = pop stack1
        heap1 = case arg1Node of
            NData tag cmpnts
                | tag == 0 {- [] -} -> hUpdate state.heap root (NInd arg2Addr)
                | otherwise -> case cmpnts of
                    [hd, tl]  -> hUpdate heap2 root (NAp addr tl)
                        where
                            (heap2, addr) = hAlloc state.heap (NAp arg3Addr hd)

primAbort :: TiState -> TiState
primAbort = error "Program abort!"

primStop :: TiState -> TiState
primStop state
    | not (isEmptyStack state.dump) = error "primStop: dump is not empty"
    | otherwise = setRuleId 11
                $ state { stack = discard state.stack.curDepth state.stack }

primPrint :: TiState -> TiState
primPrint state
    | argsLen /= 2 = error "primPrint: wrong number of args"
    | not (isEmptyStack state.dump) = error "primPrint: dump is not empty"
    | otherwise = case arg1Node of
        NNum m    -> setRuleId 12 $ state { output = state.output ++ [m]
                                          , stack = singletonStack arg2Addr }
        NData _ _ -> error "primPrint: not a number"
        _         -> setRuleId 13 $ state { stack = singletonStack arg1Addr
                                          , dump = push stack1 state.dump }
    where
        args = getargs state.heap state.stack
        argsLen = length args
        [arg1Addr, arg2Addr] = args
        arg1Node = hLookup state.heap arg1Addr
        NNum arg1Value = arg1Node
        stack1 = discard argsLen state.stack

dataStep :: Tag -> [Addr] -> TiState -> TiState
dataStep tag contents state = state { stack = stack1, dump = dump1 }
    where
        (stack1, dump1) = pop state.dump

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
instantiateConstr heap env tag arity = hAlloc heap (NPrim "Constr" (PrimConstr tag arity))

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
    = hUpdate heap updAddr (NPrim "Constr" (PrimConstr tag arity))

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

--
emptyStack' :: TiStack -> TiStack
emptyStack' stack = stack { curDepth = 0, stkItems = [] }

saveAndPush :: Addr -> TiStack -> TiDump -> (TiStack, TiDump)
saveAndPush addr stack dump
    = (push addr (emptyStack' stack), push stack dump)

restore :: TiStack -> TiDump -> (TiStack, TiDump)
restore stack dump
    = case pop dump of
        (stack', dump') 
            -> ( stack' { maxDepth = stack.maxDepth `max` stack'.maxDepth }
               , dump' )
