-- # Lifter.Mark4.Collect
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
module Lifter.Mark4.Collect
    where

import Control.Arrow
import Data.List

import Language
import Utils

collectSCs :: CoreProgram -> CoreProgram
collectSCs = concatMap collectOneSc

collectOneSc :: (Name, [Name], CoreExpr) -> [(Name, [Name], CoreExpr)]

collectOneSc = \ case
    (scName, args, rhs) -> case rhs of
        ELet False [(name1, ELam args' body)] (EVar name2)
            | name1 == name2 
                -> (scName, args ++ args', body') : scs'
                    where
                        (scs', body') = collectSCsExpr body
        _       -> (scName, args, rhs') : scs
        where
            (scs, rhs') = collectSCsExpr rhs

collectSCsExpr :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCsExpr = cataExpr phi where
    phi = \ case
        EVarF v      -> ([], EVar v)
        ENumF n      -> ([], ENum n)
        EConstrF t a -> ([],EConstr t a)
        EApF (scs1, e1) (scs2, e2)
            -> (scs1 ++ scs2, EAp e1 e2)
        ELetF isRec defns (bodySCs, body)
            -> (rhssSCs ++ bodySCs ++ localSCs, mkELet isRec nonSCs body) where
                rhssSCs  = concatMap (fst . snd) defns
                defns'   = map (second snd) defns
                (scs, nonSCs) = partition (isELam . snd) defns'
                localSCs = [ (x,xs,e) | (x, ELam xs e) <- scs ]
        ECaseF (exprSCs, e) alts 
            -> (exprSCs ++ altsSCs, ECase e alts') where
                altsSCs = concatMap (fst . thd3) alts
                alts'   = map (third snd) alts
        ELamF args (bodySCs, body)
            -> (bodySCs, ELam args body)

mkELet :: IsRec -> [(a, Expr a)] -> Expr a -> Expr a
mkELet _ [] body = body
mkELet isRec defns body = ELet isRec defns body

isELam :: Expr a -> Bool
isELam = \ case
    ELam _ _ -> True
    _        -> False
