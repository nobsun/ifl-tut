module Template.Mark4 where

import Data.List

import Utils
import Heap
import Stack
import Iseq
import Language

-- Mark 3 : Adding updating

--- Structure of the implementation

run :: String -> String
run = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = Stack Addr

type TiDump = Stack TiStack

initialTiDump :: TiDump
initialTiDump = emptyStack

type TiHeap = Heap Node
data Node = NAp Addr Addr                   -- ^ Application
          | NSupercomb Name [Name] CoreExpr -- ^ Supercombinator
          | NNum Int                        -- ^ Number
          | NInd Addr                       -- ^ Indirection
          | NPrim Name Primitive
          deriving Show

data Primitive
    = Neg
    | Add
    | Sub
    | Mul
    | Div
    deriving Show

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
  (stack, dump, heap, scDefs, stats) -> (stack, dump, heap, scDefs, f stats)

-- compiler

compile :: CoreProgram -> TiState
compile prog
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = prog ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = push addressOfMain emptyStack
      addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs
  = (heap'', scAddrs ++ primAddrs)
    where
      (heap',  scAddrs)   = mapAccumL allocateSc hInitial scDefs
      (heap'', primAddrs) = mapAccumL allocatePrim heap' primitives

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add), ("-", Sub)
             , ("*", Mul), ("/", Div)
             ]

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
  (Stack _ _ [soleAddr], dump , heap, _, _) 
    | isEmptyStack dump -> isDataNode (hLookup heap soleAddr)
  (Stack _ _ [], _, _, _, _)             -> error "Empty stack!"
  _                                      -> False

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _ -> True
  _      -> False

step :: TiState -> TiState
step state = case state of
  (stack, dump, heap, globals, stats) -> dispatch (hLookup heap (fst (pop stack)))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NInd a)                  = indStep state a
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = doAdminScSteps (scStep  state sc args body)
    dispatch (NPrim name prim)         = doAdminPrimSteps (primStep state prim)

numStep :: TiState -> Int -> TiState
numStep state n = case state of
  (stack, dump, heap, globals, stats)
    | isEmptyStack dump -> error "Number applied as a function"
    | otherwise         -> case pop dump of
      (stack', dump')     -> (stack', dump', heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (stack, dump, heap, globals, stats) -> apDispatch (hLookup heap a2)
    where
      apDispatch (NInd a3) = (stack, dump, heap', globals, stats)
        where
          heap' = hUpdate heap apNode (NAp a1 a3)
          (apNode, _) = pop stack
      apDispatch node = (push a1 stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (stack, dump, heap, globals, stats)
    | depth_ stack < argsLen + 1 -> error "Too few argments ginven"
    | otherwise                  -> (stack', dump, heap', globals, stats)
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
  (stack, dump, heap, globals, stats)
    -> (push a (discard 1 stack), dump, heap, globals, stats)

primStep :: TiState -> Primitive -> TiState
primStep state prim = case prim of
  Neg -> primNeg state
  Add -> primArith state (+)
  Sub -> primArith state (-)
  Mul -> primArith state (*)
  Div -> primArith state div

primNeg :: TiState -> TiState
primNeg state = case state of
  (stack, dump, heap, globals, stats)
    | length args /= 1         -> error "primNeg: wrong number of args"
    | not (isDataNode argNode) -> (push argAddr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise                -> (stack', dump, heap', globals, stats)
    where
      args@[argAddr] = getargs heap stack
      argNode = hLookup heap argAddr
      NNum argValue = argNode
      (_,stack') = pop stack
      (rootOfRedex,_) = pop stack'
      heap' = hUpdate heap rootOfRedex (NNum (negate argValue))

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state op = case state of
  (stack, dump, heap, globals, stats)
    | length args /= 2 -> error "primArith: wrong number of args"
    | not (isDataNode arg1Node) -> (push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
    | not (isDataNode arg2Node) -> (push arg2Addr emptyStack, push stack' dump, heap, globals, stats)
    | otherwise                 -> (stack', dump, heap', globals,stats)
    where
      args@[arg1Addr, arg2Addr] = getargs heap stack
      [arg1Node,arg2Node] = map (hLookup heap) args
      NNum arg1Val = arg1Node
      NNum arg2Val = arg2Node
      stack' = discard 2 stack
      (rootOfRedex, _) = pop stack'
      heap' = hUpdate heap rootOfRedex (NNum (op arg1Val arg2Val))

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

instantiateConstr :: Tag -> Arity -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env
  = error "Can't instantiate constructors yet"
  
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
  EConstr tag arity
            -> error "not yet implemented"
  _         -> error "Not yet implemented"

-- Formatting the results

showResults :: [TiState] -> String
showResults states
  = iDisplay (iConcat [ iLayn (map showState states)
                      , showStats (last states)
                      ])

showState :: TiState -> IseqRep
showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline
            , showDump dump, iNewline
            , showHeap heap, iNewline
            ]

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
  NInd a    -> iStr "NInd " `iAppend` showAddr a
  NPrim name prim -> iStr ("NPrim " ++ name)

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
showStats (stack, dump, heap, globals, stats)
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

test :: String -> IO ()
test = putStrLn . showResults . eval . compile . parse

