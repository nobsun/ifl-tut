-- # Lifter.Mark4.Floating
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Floating
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Language hiding (pprint)
import Lambda
import Utils
import Iseq

import Lifter.PPrint
import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract

float :: Program  (Name, Level) -> CoreProgram
float = concatMap floatSc

-- ### 6.6.9 Floating let(rec) expression

floatSc :: ScDefn (Name, Level) -> [ScDefn Name]
floatSc scDefn = case scDefn of
    (name, [], rhs) -> [(name, [], rhs')] ++ concatMap toScs fds
        where
            (fds, rhs') = floatE rhs
            toScs (_, _, defns) = map makeSc defns
            makeSc (n, r) = (n, [], r)
    _ -> error "not yet identified MFEs"

type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]

floatE :: Expr (Name, Level) -> (FloatedDefns, Expr Name)
floatE expr = case expr of
    EVar v      -> ([], EVar v)
    EConstr t a -> ([], EConstr t a)
    ENum n      -> ([], ENum n)
    EAp e1 e2   -> (fd1 ++ fd2, EAp e1' e2')
        where
            (fd1,e1') = floatE e1
            (fd2,e2') = floatE e2
    ELam args body 
        -> (fdOuter, mkELam args' (install fdThisLevel body'))
        where
            args' = map fst args
            (_,thisLevel) = head args
            (fdBody, body') = floatE body
            (fdOuter, fdThisLevel) = partitionFloats thisLevel fdBody
    ELet isRec defns body
        -> (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body')
        where
            (bodyFloatDefns, body') = floatE body
            (rhsFloatDefns, defns') = mapAccumL floatDefn [] defns
            thisGroup = (thisLevel, isRec, defns')
            (_name,thisLevel) = head (bindersOf defns)
            floatDefn floatedDefns ((name,_level), rhs)
                = (rhsFloatDefns' ++ floatedDefns, (name, rhs'))
                where
                    (rhsFloatDefns', rhs') = floatE rhs
    ECase e alts -> floatCase e alts
        where
            floatCase _e _alts = error "floatCase: not yet implemented"

mkELam :: [Name] -> CoreExpr -> CoreExpr
mkELam args expr = case expr of
    ELam args' body -> ELam (args ++ args') body
    _               -> ELam args expr

partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
partitionFloats thisLevel fds
    = partition isThisLevel fds
    where
        isThisLevel (level, _, _) = level >= thisLevel

install :: FloatedDefns -> Expr Name -> Expr Name
install defnGroups e
    = foldr installGroup e defnGroups
    where
        installGroup (_level, isRec, defns) = ELet isRec defns
