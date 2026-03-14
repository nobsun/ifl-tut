-- # Lifter.Mark4.AddLevel
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Lifter.Mark4.AddLevel
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Language
import Pretty
import Lambda
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract
import Lifter.Mark4.Floating
import Lifter.Mark4.IdentifyMFE
import Lifter.Mark4.SeparateLambda

-- ### 6.6.6 Adding level numbers

addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
addLevels = freeToLevel . freeVars

freeToLevel :: AnnProgram Name (S.Set Name) -> AnnProgram (Name, Level) Level
freeToLevel = map freeToLevelSc

freeToLevelSc :: AnnScDefn Name (S.Set Name) -> AnnScDefn (Name, Level) Level
freeToLevelSc = \ case
    (scName, [], rhs) -> (scName, [], freeToLevelE 0 [] rhs)
    _                 -> error "separateLams not applied."

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
    _ :< EApF e1 e2   -> lv :< EApF (lv :< e1') (lv :< e2')
        where
            lv = max lv1 lv2
            lv1 :< e1' = freeToLevelE level env e1
            lv2 :< e2' = freeToLevelE level env e2
    free :< ELamF args body
        -> freeSetToLevel env free :< ELamF args' body'
        where
            body' = freeToLevelE (level + 1) (args' ++ env) body
            args' = [(arg, level+1) | arg <- args]
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

freeToLevelExpr
    :: Level                        -- ^ Level of context
    -> Assoc Name Level             -- ^ Level of in-scope names
    -> AnnExpr Name (S.Set Name)    -- ^ Input expression
    -> AnnExpr (Name, Level) Level  -- ^ Result expression
freeToLevelExpr level env annexpr
    = hyloAnnExpr phi psi (level, env, annexpr) where
        
        phi = \ case
            _ -> undefined
        psi :: (Level, Assoc Name Level, AnnExpr Name (S.Set Name))
            -> F.CofreeF (ExprF Name) (Level, Assoc Name Level)
               (Level, Assoc Name Level, AnnExpr Name (S.Set Name))
        psi = \ case
            (lv,ev,aex) -> case aex of
                _    :< ENumF n      -> (lv, ev) F.:< ENumF n
                _    :< EConstrF t a -> (lv, ev) F.:< EConstrF t a
                _    :< EVarF v      -> (lv, ev) F.:< EVarF v where
                _    :< EApF e1 e2   -> (lv, ev) F.:< EApF (lv, ev, e1)
                                                           (lv, ev, e2)
                _    :< ELamF args body
                    -> (lv, ev) F.:< ELamF args (lv',ev',body) where
                        lv' = succ lv
                        ev' = ((,lv') <$> args) ++ ev
                _    :< ELetF isRec defns body
                    -> (lv, ev) F.:< ELetF isRec defns' body' where
                        binders  = bindersOf defns
                        rhss     = rhssOf defns
                        binders' = undefined
                        defns'   = undefined
                        body'    = undefined
                        
                _ -> undefined
