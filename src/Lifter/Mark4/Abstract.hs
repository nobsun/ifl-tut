-- # Lifter.Mark4.Abstract
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Abstract
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S

import Control.Arrow
import Control.Comonad.Cofree

import Lambda
import Language
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils

import Lifter.FreeVars

abstract :: AnnProgram Name (S.Set Name) -> CoreProgram
abstract prog = [ (name, args, abstractExpr rhs)
                | (name, args, rhs) <- prog
                ]

abstractExpr :: AnnExpr Name (S.Set Name) -> CoreExpr
abstractExpr expr = case expr of
    _free :< EVarF v         -> EVar v
    _free :< ENumF n         -> ENum n
    _free :< EConstrF t a    -> EConstr t a
    _free :< EApF e1 e2      -> EAp (abstractExpr e1) (abstractExpr e2)
    _free :< ELetF isRec defns body
        -> ELet isRec (map (second abstractExpr) defns) (abstractExpr body)
    free  :< ELamF args body -> foldl' EAp sc (map EVar frees)
        where
            frees = S.toList free
            sc    = ELet nonRecursive [("sc", scRhs)] (EVar "sc")
            scRhs = ELam (frees ++ args) (abstractExpr body)
    _free :< ECaseF e alts   -> ECase e' alts'
        where
            e' = abstractExpr e
            alts' = map (third abstractExpr) alts
