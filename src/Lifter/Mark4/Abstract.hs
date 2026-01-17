-- # Lifter.Mark4.Abstract
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Abstract
    where

import Data.List
import Data.Set qualified as S

import Control.Arrow
import Control.Comonad.Cofree

import Lambda
import Language
import Utils

import Lifter.FreeVars

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


