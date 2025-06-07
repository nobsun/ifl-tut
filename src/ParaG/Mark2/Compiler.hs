{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ParaG.Mark2.Compiler
    where

import Data.Char
import Data.List

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import ParaG.Mark2.Code
import ParaG.Mark2.Node
import ParaG.Mark2.PPrint
import ParaG.Mark2.State

import Debug.Trace qualified as Deb

defaultHeapSize :: Int
defaultHeapSize = 1024 ^ (2 :: Int)

defaultThreshold :: Int
defaultThreshold = defaultHeapSize

compile :: CoreProgram -> PgmState
compile program
    = PgmState
    { ctrl      = []
    , pgmGlobal = PgmGlobalState
                { output = ""
                , heap = heap'
                , globals = globals'
                , sparks  = []
                , maxtid  = 0
                , stats   = GmStats { durations = [] }
                }
    , pgmLocals = [initialTask addr]
    }
    where
        (heap', globals') 
             = let { ?sz = defaultHeapSize; ?th = defaultThreshold }
               in buildInitialHeap program
        addr = aLookup globals' "main" (error "main undefined")

initialTask :: Addr -> PgmLocalState
initialTask addr = PgmLocalState
                 { code    = initialCode
                 , stack   = singletonStack addr
                 , dump    = emptyStack
                 , vstack  = emptyStack
                 , locks   = []
                 , clock   = 0
                 , taskid  = 0
                 , ruleid  = 0
                 }

initialCode :: GmCode
initialCode = [Eval, Print]

buildInitialHeap :: (?sz :: Int, ?th :: Int) => CoreProgram -> (GmHeap, GmEnvironment)
buildInitialHeap program
    = mapAccumL allocateSc hInitial compiled
    where
        compiled :: [GmCompiledSC]
        compiled = map compileSc 
                 $ preludeDefs ++ program ++ primitives

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, arity, instrs)
    = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal arity instrs)

primitives :: [CoreScDefn]
primitives
  = [ ("+", ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ("-", ["x", "y"], EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))
    , ("*", ["x", "y"], EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))
    , ("/", ["x", "y"], EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))
    , ("negate", ["x"], EAp (EVar "negate") (EVar "x"))
    , ("==", ["x", "y"], EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))
    , ("/=", ["x", "y"], EAp (EAp (EVar "/=") (EVar "x")) (EVar "y"))
    , (">=", ["x", "y"], EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))
    , (">", ["x", "y"], EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))
    , ("<=", ["x", "y"], EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))
    , ("<", ["x", "y"], EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))
    , ("&&", ["x", "y"], EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"))
    , ("||", ["x", "y"], EAp (EAp (EVar "||") (EVar "x")) (EVar "y"))
    , ("not", ["x"], EAp (EVar "not") (EVar "x"))
    , ("if", ["c", "t", "f"], EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))
    , ("par", ["x","y"], EAp (EAp (EVar "par") (EVar "x")) (EVar "y"))
    , ("True", [], EConstr 2 0)
    , ("False", [], EConstr 1 0)
    ]

type GmCompiledSC = (Name, Arity, GmCode)

{- |
>>> compileSc (parseSc "main = 42")
>>> compileSc (parseSc "main = 3 + 4 * 5")
>>> compileSc (parseSc "main = I 3")
("main",0,[PushInt 42,Update 0,Pop 0,Unwind])
("main",0,[PushBasic 5,PushBasic 4,Mul,PushBasic 3,Add,UpdateInt 0,Return])
("main",0,[PushInt 3,PushGlobal I,MkAp,Eval,Update 0,Pop 0,Unwind])
-}
compileSc :: CoreScDefn -> GmCompiledSC
compileSc (name, args, body)
    = (name, length args, compileR body (zip args [0 ..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

compileR :: GmCompiler
compileR expr env = case expr of
    ELet isRec defs e
        | isRec     -> compileLetRec compileR defs e env
        | otherwise -> compileLet    compileR defs e env
    EAp (EVar "negate") _
        -> compileE expr env ++ [Return]
    EAp (EVar "not") _
        -> compileE expr env ++ [Return]
    EAp (EAp (EVar op) _) _
        | op `elem` aDomain builtInDyadic
            -> compileE expr env ++ [Return]
    EAp (EAp (EAp (EVar "if") e1) e2) e3
        -> compileB e1 env ++ [Cond (compileR e2 env) (compileR e3 env)]
    EAp (EAp (EVar "par") e1) e2
        -> concat 
         [ compileC e2 env
         , [Push 0, Par]
         , compileC e1 (argOffset 1 env)
         , [MkAp, Update d, Pop d, Unwind]
         ]
    ECase e alts
        -> compileE e env ++ [CaseJump (compileAlts compileAr alts env)]
    _   -> compileE expr env ++ [Update d, Pop d, Unwind]
    where
        d = length env
compileE :: GmCompiler
compileE expr env = case expr of
        ENum n -> [PushInt n]
        ELet isRec defs e
            | isRec     -> compileLetRec compileE defs e env ++ [Slide (length defs)]
            | otherwise -> compileLet    compileE defs e env ++ [Slide (length defs)]
        ECase e alts    -> compileE e env ++ [CaseJump (compileAlts compileAe alts env)]
        EAp (EAp (EVar op) _) _
            | op `elem` aDomain builtInDyadicInt   -> compileB expr env ++ [UpdateInt m]
            | op `elem` aDomain builtInDyadicBool  -> compileB expr env ++ [UpdateBool m]
            | op `elem` aDomain builtInDyadicLogic -> compileB expr env ++ [UpdateBool m]
        EAp (EVar "negate") _ -> compileB expr env ++ [UpdateInt m]
        EAp (EVar "not") _    -> compileB expr env ++ [UpdateBool m]
        EAp (EAp (EAp (EVar "if") e1) e2) e3
            -> compileB e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
        EAp (EAp (EVar "par") e1) e2
            -> concat
             [ compileC e2 env 
             , [Push 0, Par]
             , compileC e1 (argOffset 1 env)
             , [MkAp, Eval]
             ]
        _ -> compileC expr env ++ [Eval]
    where
        m = length env

compileC :: GmCompiler
compileC expr env = case expr of
    EVar v
        | v `elem` aDomain env -> [Push a]
        | otherwise            -> [PushGlobal (GlobalLabel v)]
        where
            a = aLookup env v (error "compileC: Cannot happen")
    ENum n  -> [PushInt n]
    EAp e1 e2 -> case spines expr of
        [] -> compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
        ss -> compileCS ss env
        where
            spines = iter 0 []
            iter a ss e = case e of
                EAp e1' e2'        -> iter (succ a) (e2':ss) e1'
                EConstr _ arity  
                    | a == arity -> reverse (e : ss)
                _                -> []
    ELet isRec defs e
        | isRec     -> compileLetRec compileC defs e env
        | otherwise -> compileLet compileC defs e env
    ECase e _as
            -> compileE e env -- ++ [Casejump (compileAlts as env)]
    EConstr tag 0
            -> [Pack tag 0]
    EConstr tag arity
            -> [PushGlobal (GlobalPack tag arity)]
    _       -> error $ "compileC: Not implemented for: " ++ show expr

compileB :: GmCompiler
compileB expr env = case expr of
    ENum n -> [PushBasic n]
    ELet isRec defs e
        | isRec     -> compileLetRec compileB defs e env ++ [Pop (length defs)]
        | otherwise -> compileLet compileB defs e env ++ [Pop (length defs)]
    EAp (EAp (EVar name) e1) e2 
        | name `elem` aDomain builtInDyadic
            -> compileB e2 env ++ compileB e1 env ++ [dyadic]
            where
                dyadic = aLookup builtInDyadic name (error "impossible")
    EAp (EVar "negate") e
        -> compileB e env ++ [Neg]
    EAp (EVar "not") e
        -> compileB e env ++ [Not]
    EAp (EAp (EAp (EVar "if") e1) e2) e3
        -> compileB e1 env ++ [Cond (compileB e2 env) (compileB e3 env)]
    _ -> compileE expr env ++ [Get]

builtInDyadic :: Assoc Name Instruction
builtInDyadic = builtInDyadicInt ++ builtInDyadicBool ++ builtInDyadicLogic

builtInDyadicInt :: Assoc Name Instruction
builtInDyadicInt
    = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div) ]

builtInDyadicBool :: Assoc Name Instruction
builtInDyadicBool
    = [ ("==", Eq), ("/=", Ne), (">", Gt), (">=", Ge), ("<", Lt), ("<=", Le)]

builtInDyadicLogic :: Assoc Name Instruction
builtInDyadicLogic = [ ("&&", And), ("||", Or) ]

compileLet :: GmCompiler -> Assoc Name CoreExpr -> GmCompiler
compileLet comp defs expr env
    = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where
        env' = compileArgs defs env

compileLet' :: Assoc Name CoreExpr -> GmEnvironment -> GmCode
compileLet' [] _env = []
compileLet' ((_name, expr):defs) env
    = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetRec :: GmCompiler -> Assoc Name CoreExpr -> GmCompiler
compileLetRec comp defs e env
    =  [Alloc n]
    ++ compiled defs (n-1)
    ++ comp e newArgs
    ++ [Slide n]
    where
        newArgs = compileArgs defs env
        n = length defs
        compiled dds i = case dds of
            []   -> []
            d:ds -> compileC (snd d) newArgs
                 ++ [Update i]
                 ++ compiled ds (i-1)

compileArgs :: Assoc Name CoreExpr -> GmEnvironment -> GmEnvironment
compileArgs defs env
    = zip (aDomain defs) [n-1, n-2 .. 0] ++ argOffset n env
    where
        n = length defs

compileAlts :: (Int -> GmCompiler)
            -> [CoreAlt]
            -> GmEnvironment
            -> [(Int, GmCode)]
compileAlts comp alts env
    = [ (tag, comp len body (zip names [0 ..] ++ argOffset len env))
      | (tag, names, body) <- alts, let len = length names ]

compileAe :: Int -> GmCompiler
compileAe offset expr env
    = [Split offset] ++ compileE expr env ++ [Slide offset]

compileAr :: Int -> GmCompiler
compileAr offset expr env
    = [Split offset] ++ compileR expr env


compileCS :: [CoreExpr] -> GmEnvironment -> [Instruction]
compileCS exprs env = case exprs of
    [EConstr tag arity] -> [Pack tag arity]
    e : es              -> compileC e env ++ compileCS es (argOffset 1 env)
    []                  -> error "compileCS: empty exprs"

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, m+n) | (v, m) <- env ]
