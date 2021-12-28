module Template.Mark5altprimitive where

import Data.List
import Data.Char
import Debug.Trace
import Utils
import Heap
import qualified Stack as Stk (pop)
import Stack hiding (pop)
import Iseq
import Language

--- Structure of the implementation
test, test' :: String -> IO ()
test = putStrLn . run
test' = putStrLn . run'

run, run' :: String -> String
run = showResults . eval . compile . parse
run' = showResults . eval . compile' . parse

type TiState = (Output, TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type Output  = [Int]

type TiStack = Stack Addr
pop :: TiStack -> (Addr, TiStack)
pop stk = case stk of
  _ | isEmptyStack stk -> error "Empty TiStack"
    | otherwise        -> Stk.pop stk 
  

type TiDump = Stack TiStack

initialTiDump :: TiDump
initialTiDump = emptyStack

type TiHeap = Heap Node
data Node = NAp Addr Addr                   -- ^ Application
          | NSupercomb Name [Name] CoreExpr -- ^ Supercombinator
          | NNum Int                        -- ^ Number
          | NInd Addr                       -- ^ Indirection
          | NPrim Name Primitive            -- ^ Primitive
          | NData Int [Addr]                -- ^ Tag, list of components
{- --
          deriving Show
-- -}
type Primitive = TiState -> TiState
{- --
data Primitive
    = Neg
    | Add | Sub
    | Mul | Div
    | Greater | GreaterEq
    | Less | LessEq
    | Eq | NotEq
    | PrimConstr Tag Arity
    | If
    | PrimCasePair
    | PrimCaseList
    | Print
    | Stop
    | Abort
    deriving Show
-- -}
type TiGlobals = Assoc Name Addr

data TiStats = TiStats
  { totalSteps_ :: Int
  , scSteps_    :: Int
  , primSteps_  :: Int
  }

tiStatInitial :: TiStats
tiStatInitial = TiStats 
  { totalSteps_ = 0
  , scSteps_    = 0
  , primSteps_  = 0
  }

tiStatIncTotalSteps :: TiStats -> TiStats
tiStatIncTotalSteps stats = stats { totalSteps_ = succ (totalSteps_ stats) }

tiStatGetTotalSteps :: TiStats -> Int
tiStatGetTotalSteps = totalSteps_

tiStatIncScSteps :: TiStats -> TiStats
tiStatIncScSteps stats = stats { scSteps_ = succ (scSteps_ stats) }

tiStatGetScSteps :: TiStats -> Int
tiStatGetScSteps = scSteps_

tiStatIncPrimSteps :: TiStats -> TiStats
tiStatIncPrimSteps stats = stats { primSteps_ = succ (primSteps_ stats) }

tiStatGetPrimSteps :: TiStats -> Int
tiStatGetPrimSteps = primSteps_

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = case state of
  (output, stack, dump, heap, scDefs, stats) -> (output, stack, dump, heap, scDefs, f stats)

-- compiler

compile, compile' :: CoreProgram -> TiState
compile prog
  = ([], initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = prog ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = push addressOfMain emptyStack
      addressOfMain = aLookup globals "main" (error "main is not defined")
compile' prog
  = ([], initialStack, initialTiDump, initialHeap', globals, tiStatInitial)
    where
      scDefs = prog ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = push printAddr emptyStack
      addressOfMain = aLookup globals "main" (error "main is not defined")
      addressOfPrint = aLookup globals "printList" (error "printList is not defined")
      (initialHeap', printAddr) = hAlloc initialHeap (NAp addressOfPrint addressOfMain)

extraPreludeDefs :: CoreProgram
extraPreludeDefs = 
  [ ("False",     [], EConstr 1 0)
  , ("True",      [], EConstr 2 0)
  , ("not",       ["x"],        EAp (EAp (EAp (EVar "if") (EVar "x"))
                                         (EVar "False"))
                                    (EVar "True"))
  , ("and",       ["x", "y"],    EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EVar "y"))
                                     (EVar "False"))
  , ("or",        ["x", "y"],    EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EVar "True"))
                                     (EVar "y"))
  , ("xor",       ["x", "y"],    EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EAp (EVar "not") (EVar "y")))
                                     (EVar "y"))
  , ("MkPair",    [], EConstr 1 2)
  , ("fst",       ["p"],  EAp (EAp  (EVar "casePair") (EVar "p"))
                              (EVar "K"))
  , ("snd",       ["p"],  EAp (EAp  (EVar "casePair") (EVar "p"))
                              (EVar "K1"))
  , ("Cons",      [], EConstr 2 2)
  , ("Nil",       [], EConstr 1 0)
  , ("head",      ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                   (EVar "abort"))
                              (EVar "K"))
  , ("tail",      ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                   (EVar "abort"))
                              (EVar "K1"))
  , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) 
                                   (EVar "stop"))
                              (EVar "printCons"))
  , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h"))
                                  (EAp (EVar "printList") (EVar "t")))
  ]

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs
  = (heap'', scAddrs ++ primAddrs)
    where
      (heap',  scAddrs)   = mapAccumL allocateSc hInitial scDefs
      (heap'', primAddrs) = mapAccumL allocatePrim heap' primitives

primitives :: Assoc Name Primitive
primitives = [ ("nagate", primNeg)
             , ("+", flip primArith (+)), ("-", flip primArith (-))
             , ("*", flip primArith (*)), ("/", flip primArith div)
             , (">", flip primComp (>)), (">=", flip primComp (>=))
             , ("<", flip primComp (<)), ("<=", flip primComp (<=))
             , ("==", flip primComp (==)), ("/=", flip primComp (/=))
             , ("if", primIf)
             , ("casePair", primCasePair)
             , ("caseList", primCaseList)
             , ("print", primPrint)
             , ("stop", primStop)
             , ("abort", error "program abort!")
             ]
{- --
primitives = [ ("negate", Neg)
             , ("+", Add), ("-", Sub)
             , ("*", Mul), ("/", Div)
             , (">", Greater), (">=", GreaterEq)
             , ("<", Less), ("<=", LessEq)
             , ("==", Eq), ("/=", NotEq)
             , ("if", If)
             , ("casePair", PrimCasePair)
             , ("caseList", PrimCaseList)
             , ("print", Print)
             , ("stop", Stop)
             , ("abort", Abort)
             ]
-- -}
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
  (name, args, body) -> (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NPrim name prim)

-- evaluator

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdminTotalSteps (step state)

doAdminTotalSteps :: TiState -> TiState
doAdminTotalSteps = applyToStats tiStatIncTotalSteps

doAdminScSteps :: TiState -> TiState
doAdminScSteps = applyToStats tiStatIncScSteps

doAdminPrimSteps :: TiState -> TiState
doAdminPrimSteps = applyToStats tiStatIncPrimSteps

tiFinal :: TiState -> Bool
tiFinal state = case state of
  (_, Stack _ _ [soleAddr], dump , heap, _, _) 
    | isEmptyStack dump -> isDataNode (hLookup heap soleAddr)
  (_, Stack _ _ [], _, _, _, _) -> True
  _                             -> False

-- tiFinal state = case state of
--     (_, stack, _, heap, _, _) -> case pop stack of
--         (a, stack') -> isDataNode (hLookup heap a) && isEmptyStack stack'
    
--  (_, stack, _, _, _, _) -> isEmptyStack stack

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _    -> True
  NData _ _ -> True
  _         -> False

isNumNode :: Node -> Bool
isNumNode node = case node of
  NNum _ -> True
  _      -> False

step :: TiState -> TiState
step state = case state of
  (output, stack, dump, heap, globals, stats) -> dispatch (hLookup heap (fst (pop stack)))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NInd a)                  = indStep state a
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = doAdminScSteps (scStep  state sc args body)
    dispatch (NPrim name prim)         = doAdminPrimSteps (primStep state prim)
    dispatch (NData tag compts)        = dataStep state tag compts

numStep :: TiState -> Int -> TiState
numStep state n = case state of
  (output, stack, dump, heap, globals, stats)
    | isEmptyStack dump -> error "Number applied as a function"
    | otherwise         -> case Stk.pop dump of
      (stack', dump')     -> (output, stack', dump', heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (output, stack, dump, heap, globals, stats) -> apDispatch (hLookup heap a2)
    where
      apDispatch (NInd a3) = (output, stack, dump, heap', globals, stats)
        where
          heap' = hUpdate heap apNode (NAp a1 a3)
          (apNode, _) = pop stack
      apDispatch node = (output, push a1 stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (output, stack, dump, heap, globals, stats)
    | depth_ stack < argsLen + 1 -> error "Too few argments ginven"
    | otherwise                  -> (output, stack', dump, heap', globals, stats)
    where
      argsLen   = length argNames
      stack'    = discard argsLen stack
      (root, _) = pop stack'
      heap'     = instantiateAndUpdate body root heap (bindings ++ globals)
      bindings  = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case pop stack of
  (sc, stack') -> map getarg (stack_ stack')
    where
      getarg addr = arg
        where
          NAp fun arg = hLookup heap addr

indStep :: TiState -> Addr -> TiState
indStep state a = case state of
  (output, stack, dump, heap, globals, stats)
    -> (output, push a (discard 1 stack), dump, heap, globals, stats)

primStep :: TiState -> Primitive -> TiState
primStep state prim = prim state
{- --
primStep state prim = case prim of
  Neg -> primNeg state
  Add -> primArith state (+)
  Sub -> primArith state (-)
  Mul -> primArith state (*)
  Div -> primArith state div
  Greater   -> primComp state (>)
  GreaterEq -> primComp state (>=)
  Less      -> primComp state (<)
  LessEq    -> primComp state (<=)
  Eq        -> primComp state (==)
  NotEq     -> primComp state (/=)
  If        -> primIf state
  PrimCasePair -> primCasePair state
  PrimCaseList -> primCaseList state
  PrimConstr tag arity -> primConstr state tag arity
  Print -> primPrint state
  Stop  -> primStop state
  Abort        -> error "Program abort!"
-- -}
primNeg :: TiState -> TiState
primNeg state = case state of
  (output, stack, dump, heap, globals, stats)
    | length args /= 1         -> error "primNeg: wrong number of args"
    | not (isDataNode argNode) -> (output, push argAddr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise                -> (output, stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      [argAddr] = args
      argNode = hLookup heap argAddr
      NNum argValue = argNode
      (_,stack') = pop stack
      (rootOfRedex,_) = pop stack'
      heap' = hUpdate heap rootOfRedex (NNum (negate argValue))

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state op = primDyadic state op'
  where
    op' (NNum m) (NNum n) = NNum (m `op` n)

primComp state op = primDyadic state op'
  where
    op' (NNum m) (NNum n)
      | m `op` n  = NData 2 []
      | otherwise = NData 1 []

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic state op = case state of
  (output, stack, dump, heap, globals, stats)
    | length args /= 2 -> error "primDyadic: wrong number of args"
    | not (isDataNode arg1Node) -> (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
    | not (isDataNode arg2Node) -> (output, push arg2Addr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise                 -> (output, stack', dump, heap', globals,stats)
    where
      args = getargs heap stack
      [arg1Addr, arg2Addr] = args
      [arg1Node, arg2Node] = map (hLookup heap) args
      stack' = discard 2 stack
      (rootOfRedex, _) = pop stack'
      heap' = hUpdate heap rootOfRedex (arg1Node `op` arg2Node)

primIf :: TiState -> TiState
primIf state = case state of
  (output, stack, dump, heap, globals, stats)
    | length args < 3 -> error "primIf: wrong number of args"
    | not (isDataNode arg1Node) -> (output, push arg1Addr emptyStack, push stack' dump, heap,globals, stats)
    | otherwise                 -> (output, stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      [arg1Addr,arg2Addr,arg3Addr] = take 3 args
      arg1Node = hLookup heap arg1Addr 
      stack' = discard 3 stack
      (rootOfRedex, _) = pop stack'
      result = case arg1Node of
        NData 2 [] -> arg2Addr
        _          -> arg3Addr
      heap' = hUpdate heap rootOfRedex (NInd result)

primCasePair :: TiState -> TiState
primCasePair state = case state of
  (output, stack, dump, heap, globals, stats)
    | length args < 2 -> error "primCasePair: wrong number of args"
    | not (isDataNode arg1Node) -> (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise -> (output, stack', dump, heap', globals, stats)
    where
      args@(arg1Addr:arg2Addr:_) = getargs heap stack
      arg1Node = hLookup heap arg1Addr
      stack' = discard 2 stack
      (rootOfRedex, _) = pop stack'
      heap' = case arg1Node of
        NData tag [ft,sd] -> hUpdate heap'' rootOfRedex (NAp addr sd)
          where
            (heap'',addr) = hAlloc heap (NAp arg2Addr ft)

primCaseList :: TiState -> TiState
primCaseList state = case state of
  (output, stack, dump, heap, globals, stats)
    | length args < 3 -> error "primCaseList: wrong number of args"
    | not (isDataNode arg1Node) -> (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise -> (output, stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      [arg1Addr,arg2Addr,arg3Addr] = take 3 args
      arg1Node = hLookup heap arg1Addr
      stack' = discard 3 stack
      (rootOfRedex,_) = pop stack'
      heap' = case arg1Node of
        NData tag compts
          | tag == 1  -> hUpdate heap rootOfRedex (NInd arg2Addr)
          | otherwise -> case compts of
            [hd,tl]     -> hUpdate heap'' rootOfRedex (NAp addr tl)
              where
                (heap'', addr) = hAlloc heap (NAp arg3Addr hd)

primConstr :: TiState -> Tag -> Arity -> TiState
primConstr state tag arity = case state of
  (output, stack, dump, heap, globals, stats)
    | length args < arity -> error "primConstr: wrong number of args"
    | otherwise -> (output, stack', dump, heap', globals, stats)
    where
      args = getargs heap stack
      stack' = discard arity stack
      (rootOfRedex, _) = pop stack'
      heap' = hUpdate heap rootOfRedex (NData tag args)

primConstr' :: Tag -> Arity -> TiState -> TiState
primConstr' tag arity state = primConstr state tag arity

dataStep :: TiState -> Tag -> [Addr] -> TiState
dataStep state tag compts = case state of
  (output, stack, dump, heap, globals, stats)
    | isEmptyStack dump -> (output, emptyStack, emptyStack, heap, globals, stats)
    | otherwise -> case Stk.pop dump of
        (stack', dump') -> (output, stack', dump', heap, globals, stats)

primStop :: TiState -> TiState
primStop state = case state of
    (output, stack, dump, heap, globals, stats)
      | not (isSingletonStack stack) -> error "primStop: stack is not singleton"
      | not (isEmptyStack dump)      -> error "primStop: dump is not empty"
      | otherwise -> (output, emptyStack, emptyStack , heap, globals, stats)
        
primPrint :: TiState -> TiState
primPrint state = case state of
    (output, stack, dump, heap, globals, stats)
        | {- trace (iDisplay (showStack heap stack)) -} length args /= 2  -> error "primPrint: wrong number of args"
        | otherwise -> case arg1Node of
            NNum m    -> (output ++ [m], singletonStack arg2Addr, emptyStack, heap, globals, stats)
            NData _ _ -> error "primPrint: not a number"
            _         -> (output, singletonStack arg1Addr, push stack' dump, heap, globals, stats)
      where
        args       = getargs heap stack
        [arg1Addr, arg2Addr] = args
        arg1Node   = hLookup heap arg1Addr
        stack'     = discard 2 stack 

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
  EConstr tag arity    -> error "Not implemented" -- instantiateConstr tag arity heap env
  ELet isrec defs body -> instantiateLet isrec defs body heap env
  ECase e alts         -> error "Can't instantiate case exprs"
  ELam vs e            -> error "Can't instantiate lambda abstractions"
{- --
instantiateConstr :: Tag -> Arity -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env
  = hAlloc heap (NPrim "Constructor" (PrimConstr tag arity))
-- -}
instantiateLet :: IsRec -> Assoc Name CoreExpr -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateLet isrec defs body heap env
  = instantiate body heap' env'
    where
        (heap', extraBindings) = mapAccumL instantiateRhs heap defs
        env' = extraBindings ++ env
        rhsEnv | isrec     = env'
               | otherwise = env
        instantiateRhs heap (name, rhs)
          = (heap1, (name, addr))
            where
                (heap1, addr) =instantiate rhs heap rhsEnv

instantiateAndUpdate
  :: CoreExpr         -- ^ Body of supercombinator
  -> Addr             -- ^ Address of node to update
  -> TiHeap           -- ^ Heap before instatiation
  -> Assoc Name Addr  -- ^ Associate parameters to addresses
  -> TiHeap           -- ^ Heap after instantiation
instantiateAndUpdate expr updAddr heap env = case expr of
  ENum n    -> hUpdate heap updAddr (NNum n)
  EAp e1 e2 -> hUpdate heap'' updAddr (NAp a1 a2)
    where
      (heap',  a1) = instantiate e1 heap  env
      (heap'', a2) = instantiate e2 heap' env
  EVar v    -> hUpdate heap updAddr (NInd varAddr)
    where
      varAddr = aLookup env v (error ("Undefined name " ++ show v))
  ELet isrec defs body
            -> instantiateAndUpdate body updAddr heap' env'
    where
      (heap', extraBindings) = mapAccumL instantiateRhs heap defs
      env' = extraBindings ++ env
      rhsEnv | isrec     = env'
             | otherwise = env
      instantiateRhs heap (name, rhs)
        = (heap', (name, addr))
          where
            (heap', addr) = instantiate rhs heap rhsEnv
{- -}
  EConstr tag arity
            -> instantiateAndUpdateConstr tag arity updAddr heap env
  _         -> error "Not yet implemented"

instantiateAndUpdateConstr
  :: Tag -> Arity -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdateConstr tag arity updAddr heap env
  = hUpdate heap updAddr (NPrim "Cons" (primConstr' tag arity))
-- -}
-- Formatting the results

showResults :: [TiState] -> String
showResults states
  = iDisplay (iConcat [ iLayn (map showState states)
                      , showStats (last states)
                      ])

showState :: TiState -> IseqRep
showState (output, stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline
            , showDump dump, iNewline
            , showHeap heap, iNewline
            , showOutput output, iNewline
            ]

showOutput :: Output -> IseqRep
showOutput o = iStr (show o)

showStack :: TiHeap -> TiStack -> IseqRep
showStack heap stack
  = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem (stack_ stack)))
    , iStr " ]"
    ]
    where
      showStackItem addr
        = iConcat [ showFWAddr addr, iStr ": "
                  , showStkNode heap (hLookup heap addr)
                  ]

showStkNode :: TiHeap -> Node -> IseqRep
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showAddr funAddr
            , iStr " ", showAddr argAddr, iStr " ("
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
  NInd a    -> iStr "NInd " `iAppend` showAddr a
  NPrim name prim -> iStr ("NPrim " ++ name)
  NData tag compts -> iConcat [ iStr "NData ", iNum tag, iStr " ["
                              , iInterleave (iStr ",") (map showAddr compts)
                              , iStr "]"
                              ]

showAddr :: Addr -> IseqRep
showAddr addr = iStr (showaddr addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr                                                                                                                                                                                                                         

showDump :: TiDump -> IseqRep
showDump dump = iConcat [ iStr "Dump depth ", iNum (depth_ dump)]                     

showHeap :: TiHeap -> IseqRep
showHeap heap = case contents_ heap of
  contents -> iConcat
    [ iStr "Heap  ["
    , iIndent (iInterleave iNewline (map showHeapItem contents))
    , iStr " ]"
    ]
  where
    showHeapItem (addr, node)
      = iConcat [ showFWAddr addr, iStr ": "
                , showNode node
                ]

showStats :: TiState -> IseqRep
showStats (output, stack, dump, heap, globals, stats)
  = iConcat [ iNewline
            , iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetTotalSteps stats)
            , iNewline, iStr "             Sc steps = "
            , iNum (tiStatGetScSteps stats)
            , iNewline, iStr "           Prim steps = "
            , iNum (tiStatGetPrimSteps stats)
            , iNewline, iStr "     Allocation count = "
            , iNum (allocs_ heap)
            , iNewline, iStr "   Max depth of stack = "
            , iNum (maxDepth_ stack)
            , iNewline, iStr "   Max depth of dump  = "
            , iNum (maxDepth_ dump)
            ]

-- Test(Stack a)

testProg0, testProg1, testProg2 :: String
testProg0 = "main = S K K 3"
testProg1 = "main = S K K" -- wrong (not saturated)
testProg2 = "id = S K K ;\n\
            \main = twice twice twice id 3"

testProg3 :: String
testProg3
  = unlines
  [ "pair x y f = f x y ;"
  , "fst p = p K ;"
  , "snd p = p K1 ;"
  , "f x y = letrec"
  , "            a = pair x b ;"
  , "            b = pair y a"
  , "        in"
  , "        fst (snd (snd (snd a))) ;"
  , "main = f 3 4"
  ]

testProg4 :: String
testProg4 = "main = letrec f = f x in f"

testProg5 :: String
testProg5
  = unlines
  [ "id x = x ;"
  , "main = twice twice id 3"
  ]

testProg6 :: String
testProg6
  = unlines
  [ "id x = x ;"
  , "main = twice twice twice id 3"
  ]

testProg07, testProg08, testProg09 :: String -- Ex.2.16
testProg07
  = unlines
  [ "main = negate 3"
  ]
testProg08
  = unlines
  [ "main = twice negate 3"
  ]
testProg09
  = unlines
  [ "main = negate (I 3)"
  ]

testProg10 :: String -- Ex.2.17
testProg10
  = unlines
  [ "main = 2 * 3 - 1"
  ]

testProg11 :: String -- Ex.2.21
testProg11 = unlines
  [ "fac n = if (n == 0) 1 (n * fac (n - 1)) ;"
  , "main = fac 3"
  ]

testProg12 :: String -- Ex.2.22
testProg12 = unlines
  [ "main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))"]

testProg13 :: String -- Ex.2.26
testProg13 = unlines
  [ "downfrom n = if (n == 0)"
  , "                  Nil"
  , "                  (Cons n (downfrom (n - 1))) ;"
  , "main = downfrom 4"
  ]

testProg14 :: String
testProg14 = unlines
  [ "downfrom n = if (n == 0)"
  , "                  Nil"
  , "                  (Cons n (downfrom (n - 1))) ;"
  , "main = printList (downfrom 4)"
  ]

tracing state = trace (iDisplay (showState state)) state
