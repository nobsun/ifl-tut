-- # Lifter.Mark4.Abstract
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}

module Lifter.Mark4.Abstract
    ( abstract
    ) where

import Data.Set qualified as S

import Control.Comonad.Trans.Cofree qualified as F
import Language
import Utils

abstract :: AnnProgram Name (S.Set Name) -> CoreProgram
abstract = map (third abstractExpr)

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

