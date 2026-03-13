-- # Lifter.FreeVars
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.FreeVars
    ( freeVars
    , freeVarsOf
    ) where

import Control.Arrow
import Data.Bool
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F

import Lambda
import Language
import Utils


freeVars :: CoreProgram -> AnnProgram Name (S.Set Name)
freeVars prog = [ (name, args, freeVarsExpr (S.fromList args) body)
                | (name, args, body) <- prog ]

freeVarsExpr :: S.Set Name                 -- Candidate of concerning free variables
             -> CoreExpr                   -- Expression to annotate
             -> AnnExpr Name (S.Set Name)  -- Annotated result
freeVarsExpr lvs expr = hyloAnnExpr phi psi (lvs, expr) where
    phi = \ case
        _  F.:< ENumF n       -> S.empty         :< ENumF n
        _  F.:< EConstrF t a  -> S.empty         :< EConstrF t a
        ls F.:< EVarF v
            | S.member v ls   -> S.singleton v   :< EVarF v
            | otherwise       -> S.empty         :< EVarF v
        _  F.:< EApF e1 e2    -> S.union ls1 ls2 :< EApF e1 e2 where
            ls1 = freeVarsOf e1
            ls2 = freeVarsOf e2
        _  F.:< ELamF args body
            -> freeVarsOf body S.\\ S.fromList args :< ELamF args body
        _  F.:< ELetF isRec defns body
            -> S.union defnsFree bodyFree :< ELetF isRec defns body where
                defnsFree
                    | isRec     = freeInRhss S.\\ S.fromList binders
                    | otherwise = freeInRhss
                bodyFree        = freeVarsOf body S.\\ S.fromList binders
                freeInRhss      = S.unions (map freeVarsOf rhss)
                (binders, rhss) = (bindersOf &&& rhssOf) defns
        _  F.:< ECaseF e alts 
            -> S.union eFree altsFree :< ECaseF e alts where
                eFree    = freeVarsOf e
                altsFree = S.unions (map (freeVarsOf . thd3) alts)
    psi = \ case
        (ls,ENum n)      -> ls F.:< ENumF n
        (ls,EConstr t a) -> ls F.:< EConstrF t a
        (ls,EVar v)      -> ls F.:< EVarF v
        (ls,EAp e1 e2)   -> ls F.:< EApF (ls,e1) (ls,e2)
        (ls,ELam arg e)  -> ls F.:< ELamF arg e' where
            e' = (ls `S.union` S.fromList arg, e)
        (ls,ELet isRec defns e) -> ls F.:< ELetF isRec defns' e' where
            defns' = zip binders rhss'
            e'     = (bodyLs, e)
            (binders,rhss) = (bindersOf &&& rhssOf) defns
            rhss'          = map (rhsLs,) rhss
            rhsLs | isRec     = bodyLs
                  | otherwise = ls
            bodyLs = S.union ls (S.fromList binders)
        (ls,ECase e alts) -> ls F.:< ECaseF e' alts' where
            e'    = (ls, e)
            alts' = map f alts where
                f (t,as,r) = (t,as,(S.union ls (S.fromList as), r))

freeVarsOf :: AnnExpr Name (S.Set Name) -> S.Set Name
freeVarsOf = \ case
    fvs :< _ -> fvs

-- original implementation in the text

freeVars' :: CoreProgram -> AnnProgram Name (S.Set Name)
freeVars' prog = [ (name, args, freeVarsExpr' (S.fromList args) body)
                 | (name, args, body) <- prog ]

freeVarsExpr' :: S.Set Name                 -- Candidate of concerning free variables
              -> CoreExpr                   -- Expression to annotate
              -> AnnExpr Name (S.Set Name)  -- Annotated result
freeVarsExpr' lvs = \ case
    EVar v
        | S.member v lvs  -> S.singleton v :< EVarF v
        | otherwise       -> S.empty       :< EVarF v
    ENum k                -> S.empty       :< ENumF k
    EConstr t a           -> S.empty       :< EConstrF t a
    EAp e1 e2             -> fvs           :< EApF e1' e2' where
        e1'@(fvs1 :< _) = freeVarsExpr' lvs e1
        e2'@(fvs2 :< _) = freeVarsExpr' lvs e2
        fvs             = S.union fvs1 fvs2
    ELet isRec defns body -> fvs           :< ELetF isRec defns' body' where
        binders      = S.fromList $ bindersOf defns
        bodyLvs      = S.union lvs binders
        rhsLvs       = bool lvs bodyLvs isRec
        rhss'        = map (freeVarsExpr' rhsLvs) (rhssOf defns)
        defns'       = zip (bindersOf defns) rhss'
        freeInValues = S.unions (map freeVarsOf rhss')
        defnsFree    = bool freeInValues (S.difference freeInValues binders) isRec
        body'        = freeVarsExpr' bodyLvs body
        bodyFree     = S.difference (freeVarsOf body') binders
        fvs          = S.union defnsFree bodyFree
    ECase e alts          -> freeVarsCase lvs e alts where
        freeVarsCase _lvs _e _alts = error "freeVarsExpr'.freeVarsCase: not yet implemented"
    ELam args body        -> fvs           :< ELamF args body' where
        body'@(fvs' :< _) = freeVarsExpr' lvs' body
        lvs'  = S.union lvs args'
        args' = S.fromList args
        fvs   = S.difference fvs' args'
