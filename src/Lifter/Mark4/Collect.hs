-- # Lifter.Mark4.Collect
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Collect
    where

import Data.List

import Language

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
collectSCsExpr expr = case expr of
    ENum _    -> ([], expr)
    EVar _    -> ([], expr)
    EAp e1 e2 -> (scs1 ++ scs2, EAp e1' e2')
        where
            (scs1, e1') = collectSCsExpr e1
            (scs2, e2') = collectSCsExpr e2
    ELam args body -> (scs, ELam args body')
        where
            (scs, body') = collectSCsExpr body
    EConstr _ _ -> ([], expr)
    ECase e alts -> (scsExpr ++ scsAlts, ECase e' alts')
        where
            (scsExpr, e') = collectSCsExpr e
            (scsAlts, alts') = mapAccumL collectSCsAlt [] alts
            collectSCsAlt scs (tag,args,rhs) =(scs ++ scsRhs, (tag, args, rhs'))
                where
                    (scsRhs, rhs') = collectSCsExpr rhs
    ELet isRec defns body -> (rhssSCs ++ bodySCs ++ localSCs, mkELet isRec nonSCs' body')
        where
            (rhssSCs, defns') = mapAccumL collectSCsDefn [] defns
            scs'     = [(name,rhs) | (name,rhs) <- defns', isELam rhs]
            nonSCs'  = [(name,rhs) | (name,rhs) <- defns', not (isELam rhs)]
            localSCs = [(name,args,be) | (name,ELam args be) <- scs']
            (bodySCs, body') = collectSCsExpr body
            
collectSCsDefn :: [CoreScDefn] -> (Name, CoreExpr) -> ([CoreScDefn], (Name, Expr Name))
collectSCsDefn scs (name,rhs) = case rhs of
    ELet False [(name1, ELam rhsArgs rhsBody)] (EVar name2)
        | name1 == name2 -> case collectSCsExpr rhsBody of
            (scs'', rhs'')     -> (scs ++ scs'', (name, ELam rhsArgs rhs''))
    _ -> (scs ++ rhsSCs, (name, rhs'))
    where
        (rhsSCs, rhs') = collectSCsExpr rhs

mkELet :: IsRec -> [(a, Expr a)] -> Expr a -> Expr a
mkELet _ [] body = body
mkELet isRec defns body = ELet isRec defns body

isELam :: Expr a -> Bool
isELam = \ case
    ELam _ _ -> True
    _        -> False
        
