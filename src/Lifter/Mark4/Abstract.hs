-- # Lifter.Mark4.Abstract
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Abstract
    ( abstract
    ) where

import Control.Arrow
import Data.List
import Data.Set qualified as S

import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Iseq
import Lambda
import Language
import Parse
import Pretty
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils

import Lifter.FreeVars

abstract :: AnnProgram Name (S.Set Name) -> CoreProgram
abstract = map (third abstractExpr)
{- ^ 
abstract prog = [ (name, args, abstractExpr rhs)
                | (name, args, rhs) <- prog
                ]
-}

abstractExpr :: AnnExpr Name (S.Set Name) -> CoreExpr
abstractExpr = cataAnnExpr phi where
    phi = \ case
        _    F.:< EVarF v      -> EVar v
        _    F.:< ENumF n      -> ENum n
        _    F.:< EConstrF t a -> EConstr t a
        _    F.:< EApF e1 e2   -> EAp e1 e2
        _    F.:< ELetF isRec defns body
            -> ELet isRec defns body
        _    F.:< ECaseF e alts 
            -> ECase e alts
        free F.:< ELamF args body 
            -> foldl' EAp sc (map EVar frees)
            where
                frees = S.toList free
                sc = ELet nonRecursive [("sc",scRhs)] (EVar "sc")
                scRhs = ELam (frees ++ args) body

check :: String -> IO ()
check = putStrLn 
      . pprint
      . abstract
      . freeVars . parse
