-- # Lifter.Mark3.Abstract
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark3.Abstract
    where

import Data.Char
import Data.Function
import Data.List
import Data.Set qualified as S

import Control.Arrow
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

import Lifter.Mark3.Rename
import Lifter.Mark3.Collect
import Lifter.Mark3.FreeVars

abstractJ :: AnnProgram Name (S.Set Name) -> CoreProgram
abstractJ prog = [ (name, args, abstractJExpr [] rhs)
                 | (name, args, rhs) <- prog]

abstractJExpr :: Assoc Name [Name] -> AnnExpr Name (S.Set Name) -> CoreExpr
abstractJExpr env = \ case
    _free :< EVarF g         -> foldl' EAp (EVar g) (map EVar (aLookup env g []))
    _free :< ENumF n         -> ENum n
    _free :< EConstrF t a    -> EConstr t a
    _free :< EApF e1 e2      -> EAp (abstractJExpr env e1) (abstractJExpr env e2)
    free  :< ELamF args body -> foldl' EAp sc (map EVar frees)
        where
            frees = actualFrees env free
            sc    = ELet False [("sc", scRhs)] (EVar "sc")
            scRhs = ELam (frees ++ args) (abstractJExpr env body)
    _free :< ELetF isRec defns body 
        -> ELet isRec (funDefns' ++ varDefns') body'
        where
            (funDefns,varDefns) = partition (isALam . snd) defns
            funNames = bindersOf funDefns
            freeInFuns = S.difference (S.unions (freeVarsOf . snd <$> funDefns))
                                      (S.fromList funNames)
            varsToAbstract = actualFrees env freeInFuns
            freesInFuns = actualFreesOfBindings env (map (second freeVarsOf) funDefns)
            bodyEnv
                | isRec     = map (second S.toList) freesInFuns
                | otherwise = map (,varsToAbstract) funNames ++ env
            rhsEnv | isRec     = bodyEnv
                   | otherwise = env
            funDefns'
                | isRec
                    = [ (name, ELam (S.toList (aLookup freesInFuns name S.empty) ++ args)
                                    (abstractJExpr rhsEnv fnbody))
                      | (name, _ :< ELamF args fnbody) <- funDefns ]
                | otherwise
                    = [ (name, ELam (varsToAbstract ++ args) 
                                    (abstractJExpr rhsEnv fnBody))
                      | (name, _ :< ELamF args fnBody) <- funDefns ]
            varDefns' = second (abstractJExpr rhsEnv) <$> varDefns
            body' = abstractJExpr bodyEnv body
    _free :< ECaseF e alts   -> ECase e' alts'
        where
            e'    = abstractJExpr env e
            alts' = [ (tag, args, abstractJExpr env body')
                    | (tag, args, body') <- alts ]

isALam :: AnnExpr a ann -> Bool
isALam = \ case
    _ :< ELamF _ _ -> True
    _              -> False

actualFrees :: Assoc Name [Name] -> S.Set Name -> [Name]
actualFrees env free
    = S.toList 
    $ S.unions 
    [ S.fromList (aLookup env name [name])
    | name <- S.toList free ]

actualFreesOfBindings
    :: Assoc Name [Name] -> Assoc Name (S.Set Name) -> Assoc Name (S.Set Name)
actualFreesOfBindings env bs
    | canImprove = actualFreesOfBindings env bs'
    | otherwise  = map (second (flip S.difference (S.fromList funNames))) bs
    where
        funNames = bindersOf bs
        canImprove = any (not . S.null . uncurry S.difference . (snd *** snd)) 
                         (zip bs bs')
        bs' = improveFreesOfBindings env bs

improveFreesOfBindings
    :: Assoc Name [Name] -> Assoc Name (S.Set Name) -> Assoc Name (S.Set Name)
improveFreesOfBindings env bs
    = map (second (S.fromList . actualFrees env')) bs
    where
        env' = map (second S.toList) bs ++ env

{- --
abstract :: [(Name, [Name], AnnExpr Name (S.Set Name))] -> [(Name, [Name], CoreExpr)]
abstract prog = [ (name, args, abstractExpr rhs)
                | (name, args, rhs) <- prog
                ]

abstractExpr :: AnnExpr Name (S.Set Name) -> CoreExpr
abstractExpr = \ case
    _free :< EVarF v -> EVar v
    _free :< ENumF n -> ENum n
    _free :< EConstrF t a -> EConstr t a
    _free :< EApF e1 e2 -> EAp (abstractExpr e1) (abstractExpr e2)
    _free :< ELetF isRec defns body
        -> ELet isRec (map (second abstractExpr) defns) (abstractExpr body)
    free  :< ELamF args body
        -> foldl' EAp sc (map EVar fvs)
        where
            fvs = S.toList free
            sc = ELet False [("sc", scRhs)] (EVar "sc")
            scRhs = ELam (fvs ++ args) (abstractExpr body)
    _free :< ECaseF e alts -> abstractCase e alts

abstractCase :: AnnExpr Name (S.Set Name) -> AnnAlters Name (S.Set Name) -> CoreExpr
abstractCase e alts = ECase (abstractExpr e) (map abstractAlter alts)

abstractAlter :: AnnAlter Name (S.Set Name) -> CoreAlt
abstractAlter (tag, args, rhs) = (tag, args, abstractExpr rhs)
-- -}
