-- # Lifter.Mark4.Floating
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.Floating
    where

import Control.Arrow
import Data.List
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Language

import Lifter.Mark4.Rename

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
    ECase e alts -> (eds ++ altds, ECase e' alts')
        where
            (eds, e')      = floatE e
            (altds, alts') = first concat $ unzip $ map floatAlt alts
            floatAlt (tag, args, rhs) = case floatE rhs of
                (fds, rhs')
                    | null args -> (fds, (tag, [], rhs'))
                    | otherwise -> (fdsOuter, (tag, args', install fdThisLevel rhs'))
                    where
                        args'                   = map fst args
                        thisLevel               = snd (head args)
                        (fdsOuter, fdThisLevel) = partitionFloats thisLevel fds

mkELam :: [Name] -> CoreExpr -> CoreExpr
mkELam args expr = case expr of
    ELam args' body -> ELam (args ++ args') body
    _               -> ELam args expr

partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
partitionFloats thisLevel fds
    = partition isThisLevel fds
    where
        isThisLevel (level, _, _) = level < thisLevel

install :: FloatedDefns -> Expr Name -> Expr Name
install defnGroups e
    = foldr installGroup e defnGroups
    where
        installGroup (_level, isRec, defns) = ELet isRec defns
