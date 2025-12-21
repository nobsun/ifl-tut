-- # Lifter.Mark3.FreeVars
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark3.FreeVars
    where

import Data.Set qualified as S
import Control.Comonad.Cofree

import Lambda
import Language

freeVars :: CoreProgram -> AnnProgram Name (S.Set Name)
freeVars prog = [ (name, args, freeVarsExpr (S.fromList args) body)
                | (name, args, body) <- prog ]

freeVarsExpr :: S.Set Name                 -- Candidates for free variables
             -> CoreExpr                   -- Expression to annotate
             -> AnnExpr Name (S.Set Name)  -- Annotated result
freeVarsExpr lv = paraExpr phi where
    phi :: ExprF Name (CoreExpr, AnnExpr Name (S.Set Name)) -> AnnExpr Name (S.Set Name)
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
        ECaseF (_,e') alts -> S.union eFree altsFree :< ECaseF e' alts'
            where
                eFree = freeVarsOf e'
                altsFree = S.unions (freeVarsOf . rhsOfAlter <$> alts')
                alts' = map freeVarsAlter alts
                freeVarsAlter (tag,args,(rhs, _)) = (tag,args,freeVarsExpr rhsLv rhs)
                    where
                        rhsLv = S.union (S.fromList args) lv
                rhsOfAlter (_,_,rhs) = rhs

        ELamF args (_,body) -> (freeVarsOf body S.\\ S.fromList args) :< ELamF args body

freeVarsOf :: AnnExpr Name (S.Set Name) -> S.Set Name
freeVarsOf = \ case
    fvs :< _ -> fvs
