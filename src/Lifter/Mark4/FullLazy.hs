-- # Lifter.Mark4.FullLazy
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.FullLazy
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Language hiding (pprint, )
import Lambda
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.PPrint
import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract
import Lifter.Mark4.Floating

fullyLazyLift :: CoreProgram -> CoreProgram
fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams

separateLams :: CoreProgram -> CoreProgram
separateLams prog
    = [ (name, [], mkSepLams args (separateLamsE rhs))
      | (name, args, rhs) <- prog ]

-- type Level = Int

addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
addLevels = freeToLevel . freeVars

identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)
identifyMFEs = map (third (identifyMFEsE 0))

freeToLevel :: AnnProgram Name (S.Set Name) -> AnnProgram (Name, Level) Level
freeToLevel = map freeToLevelSc

freeToLevelSc :: AnnScDefn Name (S.Set Name) -> AnnScDefn (Name, Level) Level
freeToLevelSc = \ case
    (scName, [], rhs) -> (scName, [], freeToLevelE 0 [] rhs)
    _                 -> error "separateLams not applied."

-- ### 6.6.5 Separating the lambdas

separateLamsE :: CoreExpr -> CoreExpr
separateLamsE = \ case
    e@(EVar _)      -> e
    e@(EConstr _ _) -> e
    e@(ENum _)      -> e
    EAp e1 e2       -> EAp (separateLamsE e1) (separateLamsE e2)
    ECase e alts    -> ECase (separateLamsE e) (third separateLamsE <$> alts)
    ELam args body  -> mkSepLams args (separateLamsE body)
    ELet isRec defns body
        -> ELet isRec (second separateLamsE <$> defns)
                      (separateLamsE body)

mkSepLams :: [a] -> Expr a -> Expr a
mkSepLams args body 
    = foldr mkSepLam body args
    where
        mkSepLam arg = ELam [arg]

-- ### 6.6.6 Adding level numbers

freeSetToLevel :: Assoc Name Level -> S.Set Name -> Level
freeSetToLevel env free
    = foldl' max 0 [ aLookup env n 0 | n <- S.toList free ]

freeToLevelE
    :: Level                        -- ^ Level of context
    -> Assoc Name Level             -- ^ Level of in-scope names
    -> AnnExpr Name (S.Set Name)    -- ^ Input expression
    -> AnnExpr (Name, Level) Level  -- ^ Result expression

freeToLevelE level env = \ case
    _ :< ENumF k      -> 0 :< ENumF k
    _ :< EVarF v      -> aLookup env v 0 :< EVarF v
    _ :< EConstrF t a -> 0 :< EConstrF t a
    _ :< EApF e1 e2    -> max lv1 lv2 :< EApF e1' e2'
        where
            e1'@(lv1 :< _) = freeToLevelE level env e1
            e2'@(lv2 :< _) = freeToLevelE level env e2
    free :< ELamF args body
        -> freeSetToLevel env free :< ELamF args' body'
        where
            body' = freeToLevelE (level + 1) (args' ++ env) body
            args' = [(arg, level +1) | arg <- args]
    _ :< ELetF isRec defns body
        -> lvbody :< ELetF isRec defns' body'
        where
            binders  = bindersOf defns
            rhss     = rhssOf defns

            binders' = map (, maxRhsLevel) binders
            rhss'    = map (freeToLevelE level rhsEnv) rhss
            defns'   = zip binders' rhss'
            body'@(lvbody :< _) = freeToLevelE level bodyEnv body

            freeInRhss = S.unions $ [ free | free :< _ <- rhss ]
            maxRhsLevel = freeSetToLevel levelRhsEnv freeInRhss

            bodyEnv = binders' ++ env
            rhsEnv | isRec     = bodyEnv
                   | otherwise = env
            levelRhsEnv | isRec     = map (, 0) binders ++ env
                        | otherwise = env
    free :< ECaseF e alts
        -> freeToLevelCase level env free e alts
        where
            freeToLevelCase _ _ _ = error "freeToLevelCase: not yet implemented"

-- ### 6.6.7 Identifying MFEs

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

