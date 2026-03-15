-- # Lifter.Mark4.IdentifyMFE
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.IdentifyMFE
    where

import Data.Set qualified as S
import Control.Comonad.Cofree

import Language
import Lambda
import Utils

import Lifter.Mark4.Rename

-- ### 6.6.7 Identifying MFEs

identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)
identifyMFEs = map (third (identifyMFEsE 0))

{- -}
identifyMFEsE :: Level                        -- Level of context
              -> AnnExpr (Name, Level) Level  -- Input expression
              -> Expr (Name, Level)           -- Result
identifyMFEsE ctx = \ case
    ae@(level :< _)
        | level == ctx || notMFECandidate ae -> e
        | otherwise                          -> transformMFE level e
        where
            e = identifyMFEsE1 ae

notMFECandidate :: AnnExpr a ann -> Bool
notMFECandidate = \ case
    _ :< EConstrF _ _ -> True
    _ :< ENumF _      -> True
    _ :< EVarF _      -> True
    _                 -> False  -- For now everything else is a candidate
-- -}

identifyMFEsE' :: Level                                -- Level of context
               -> AnnExpr (Name, Level) (Bool, Level)  -- Input expression
               -> AnnExpr (Name, Level) (Bool, Level)  -- Result
identifyMFEsE' ctx = \ case
    ae@((_,level) :< _)
        | level == ctx || notMFECandidate' ae -> ae
        | otherwise -> transformMFE' ae

notMFECandidate' :: AnnExpr a (Bool, ann) -> Bool
notMFECandidate' = \ case
    bann :< _ -> fst bann

{- -}
transformMFE :: Level -> Expr (Name, Level) -> Expr (Name, Level)
transformMFE level e = ELet nonRecursive [(("v", level), e)] (EVar "v")
-- -}

transformMFE' :: AnnExpr (Name, Level) (Bool, Level) -> AnnExpr (Name, Level) (Bool, Level)
transformMFE' ae = case ae of
    (True, _)  :< _ -> ae
    (_, _)  :< _ -> undefined

{- -}
identifyMFEsE1 :: AnnExpr (Name, Level) Level
               -> Expr (Name,Level)
identifyMFEsE1 = \ case
    _ :< EConstrF t a   -> EConstr t a
    _ :< ENumF n        -> ENum n
    _ :< EVarF v        -> EVar v
    level :< EApF e1 e2 -> EAp (identifyMFEsE level e1)
                               (identifyMFEsE level e2)
    _ :< ELamF args body
        -> ELam args (identifyMFEsE argLevel body)
        where 
            (_, argLevel) = head args
    level :< ELetF isRec defns body
        -> ELet isRec defns' body'
        where
            body' = identifyMFEsE level body
            defns' = [ ((name, rhsLevel), identifyMFEsE rhsLevel rhs)
                     | ((name, rhsLevel), rhs) <- defns ]
    level :< ECaseF e alts -> identifyMFEsCase1 level e alts
        where
            identifyMFEsCase1 = error "identifyMFEsCase1: not yet implemented"
-- -}

identifyMFEsE1' :: AnnExpr (Name, Level) (Bool, Level)
                -> AnnExpr (Name, Level) (Bool, Level)
identifyMFEsE1' ae = case ae of
    _ :< EConstrF _ _   -> ae
    _ :< ENumF _        -> ae
    _ :< EVarF _        -> ae
    _ :< EApF _e1 _e2 -> undefined
    _ :< ELamF _args _body -> undefined
    _ :< ELetF _isRec _defns _body -> undefined
    _ :< ECaseF _e _alts -> undefined

