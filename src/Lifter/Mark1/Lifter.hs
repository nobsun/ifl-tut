-- # Lifter.Mark1.Lifter
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark1.Lifter
    where

import Data.Char
import Data.Function
import Data.List
import Data.Set qualified as S
-- import Text.ParserCombinators.ReadP
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Lambda
import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.Mark1.Rename

run :: String -> ([String] -> [String])
run prog inputs 
    = Gm7.showFullResults 
    $ Gm7.eval 
    $ Gm7.setControl inputs
    $ Gm7.compile
    $ lambdaLift
    $ parse prog

runS :: String -> String
runS = pprint . lambdaLift . parse 

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

freeVars :: CoreProgram -> AnnProgram Name (S.Set Name)
freeVars prog = [ (name, args, freeVarsExpr (S.fromList args) body)
                | (name, args, body) <- prog ]

abstract :: AnnProgram Name (S.Set Name) -> CoreProgram
abstract prog = [ (scName, args, abstractExpr rhs) 
                | (scName, args, rhs) <- prog ]

freeVarsExpr :: S.Set Name                 -- Candidates for free variables
             -> CoreExpr                   -- Expression to annotate
             -> AnnExpr Name (S.Set Name)  -- Annotated result
freeVarsExpr lv = paraExpr phi where
    phi = \ case
        EVarF v
            | S.member v lv -> S.singleton v :< EVarF v
            | otherwise     -> S.empty :< EVarF v
        ENumF n -> S.empty :< ENumF n
        EConstrF tag arity -> freeVarsExpr rlv (ELam llv' (foldl' EAp (EConstr tag arity) (map EVar llv')))
            where
                (llv,rlv) = S.splitAt arity lv
                llv' = S.toList llv
        EApF (_,e1) (_,e2) -> (freeVarsOf e1 `S.union` freeVarsOf e2) :< EApF e1 e2
        ELetF isRec defns (body,_) -> (defnsFree `S.union` bodyFree) :< ELetF isRec defns' body'
            where
                binders = bindersOf defns
                binderSet = S.fromList binders
                bodyLv = lv `S.union` binderSet
                rhsLv | isRec     = bodyLv
                      | otherwise = lv
                rhss' = map (freeVarsExpr rhsLv . fst) (rhssOf defns)
                defns' = zip binders rhss'
                freeInValues = S.unions (map freeVarsOf rhss')
                defnsFree | isRec = freeInValues S.\\ binderSet
                          | otherwise = freeInValues
                body' = freeVarsExpr bodyLv body
                bodyFree = freeVarsOf body' S.\\ binderSet
        ECaseF _e _alters -> error "freeVarsExpr: not yet implemented on ECase"
        ELamF args (_,body) -> (freeVarsOf body S.\\ S.fromList args) :< ELamF args body

freeVarsOf :: AnnExpr Name (S.Set Name) -> S.Set Name
freeVarsOf = \ case
    fvs :< _ -> fvs

abstractExpr :: AnnExpr Name (S.Set Name) -> CoreExpr
abstractExpr = cataAnnExpr phi where
    phi = \ case
        _free F.:< EVarF v -> EVar v
        _free F.:< ENumF n -> ENum n
        _free F.:< EConstrF tag arity -> EConstr tag arity
        _free F.:< EApF e1 e2 -> EAp e1 e2
        _free F.:< ELetF isRec defns e -> ELet isRec defns e
        free  F.:< ELamF args body -> foldl' EAp sc (map EVar fvList) where
            fvList = S.toList free
            sc = ELet nonRecursive [("sc", scRhs)] (EVar "sc")
            scRhs = ELam (fvList ++ args) body
        _ -> error "Both of EConstr and ECase are not yet implemented"
