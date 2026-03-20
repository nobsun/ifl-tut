-- # Lifter.Mark4.SeparateLambda
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.SeparateLambda
    ( separateLams
    ) where

import Data.Functor.Foldable

import Language

-- ### 6.6.5 Separating the lambdas

separateLams :: CoreProgram -> CoreProgram
separateLams prog
    = [ (name, [], mkSepLams args (separateLamsExpr rhs))
      | (name, args, rhs) <- prog ]

separateLamsExpr :: CoreExpr -> CoreExpr
separateLamsExpr = cataExpr phi where
    phi = \ case
        EVarF v         -> EVar v
        ENumF n         -> ENum n
        EConstrF t a    -> EConstr t a
        EApF e1 e2      -> EAp e1 e2
        ECaseF e alts   -> ECase e alts
        ELamF args body -> mkSepLams args body
        ELetF isRec defns body -> ELet isRec defns body

mkSepLams :: [a] -> Expr a -> Expr a
mkSepLams args body = cata phi args where
    phi = \ case
        Nil            -> body
        Cons arg body' -> ELam [arg] body'
