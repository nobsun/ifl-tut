{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Lambda
    where

import GHC.Generics (Generic)
import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable
import Data.Bool
import Data.Char
import Data.Maybe
import Language

data ExprF a r
    = EVarF Name
    | ENumF Int
    | EConstrF
        Tag
        Arity
    | EApF r r
    | ELetF
        IsRec
        (BindersF a r)
        r
    | ECaseF
        r 
        (AltersF a r)
    | ELamF
        [a]
        r
    deriving (Eq, Show, Functor, Generic)

type BindersF a r = [(a, r)]

type AltersF a r = [(Tag, [a], r)]

type instance Base (Expr a) = ExprF a

instance Recursive (Expr a) where
    project :: Expr a -> Base (Expr a) (Expr a)
    project = \ case
        EVar n -> EVarF n
        ENum n -> ENumF n
        EConstr tag ary -> EConstrF tag ary
        EAp e1 e2       -> EApF e1 e2
        ELet isrec bs e -> ELetF isrec bs e
        ECase e alts    -> ECaseF e alts
        ELam xs e       -> ELamF xs e

cataExpr :: (ExprF a b -> b) -> Expr a -> b
cataExpr = cata

paraExpr :: (ExprF a (Expr a, b) -> b) -> Expr a -> b
paraExpr = para

anaExpr :: (b -> ExprF a b) -> b -> Expr a
anaExpr = ana

apoExpr :: (b -> ExprF a (Either (Expr a) b)) -> b -> Expr a
apoExpr = apo

instance Corecursive (Expr a) where
    embed :: Base (Expr a) (Expr a) -> Expr a
    embed = \ case
        EVarF n -> EVar n
        ENumF n -> ENum n
        EConstrF tag ary -> EConstr tag ary
        EApF e1 e2       -> EAp e1 e2
        ELetF isrec bs e -> ELet isrec bs e
        ECaseF e alts    -> ECase e alts
        ELamF xs e       -> ELam xs e


{- AnnProgram -}

type AnnProgram a ann = [AnnScDefn a ann]
type AnnScDefn a ann = (Name, [a], AnnExpr a ann)
type AnnExpr a = Cofree (ExprF a)
type AnnBinders a ann = [(a, AnnExpr a ann)]

cataAnnExpr :: (F.CofreeF (ExprF a) ann b -> b)
            -> AnnExpr a ann -> b
cataAnnExpr = cata

paraAnnExpr :: (F.CofreeF (ExprF a) ann (AnnExpr a ann, b) -> b)
            -> AnnExpr a ann -> b
paraAnnExpr = para

anaAnnExpr :: (b -> F.CofreeF (ExprF a) ann b)
           -> b -> AnnExpr a ann
anaAnnExpr = ana

apoAnnExpr :: (b -> F.CofreeF (ExprF a) ann (Either (AnnExpr a ann) b))
           -> b -> AnnExpr a ann
apoAnnExpr = apo

