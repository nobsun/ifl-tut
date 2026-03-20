{-# LANGUAGE GHC2021 #-}
{-  LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Lambda
    where

-- import GHC.Generics (Generic)
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
    deriving (Eq, Show, Functor)

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

zygoExpr :: (ExprF a c -> c) -> (ExprF a (c, b) -> b) -> Expr a -> b
zygoExpr = zygo

anaExpr :: (b -> ExprF a b) -> b -> Expr a
anaExpr = ana

apoExpr :: (b -> ExprF a (Either (Expr a) b)) -> b -> Expr a
apoExpr = apo

hyloExpr :: (ExprF a c -> c) -> (b -> ExprF a b) -> b -> c
hyloExpr = hylo

histoExpr :: (ExprF a (AnnExpr a b) -> b) -> Expr a -> b
histoExpr = histo

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
type AnnAlter a ann = (Tag, [a], AnnExpr a ann)
type AnnAlters a ann = [AnnAlter a ann]

cataAnnExpr :: (F.CofreeF (ExprF a) ann b -> b)
            -> AnnExpr a ann -> b
cataAnnExpr = cata

paraAnnExpr :: (F.CofreeF (ExprF a) ann (AnnExpr a ann, b) -> b)
            -> AnnExpr a ann -> b
paraAnnExpr = para

zygoAnnExpr :: (F.CofreeF (ExprF a) ann c -> c)
            -> (F.CofreeF (ExprF a) ann (c, b) -> b) 
            -> AnnExpr a ann -> b
zygoAnnExpr = zygo

anaAnnExpr :: (b -> F.CofreeF (ExprF a) ann b)
           -> b -> AnnExpr a ann
anaAnnExpr = ana

apoAnnExpr :: (b -> F.CofreeF (ExprF a) ann (Either (AnnExpr a ann) b))
           -> b -> AnnExpr a ann
apoAnnExpr = apo

hyloAnnExpr :: (F.CofreeF (ExprF a) ann c -> c)
            -> (b -> F.CofreeF (ExprF a) ann b)
            -> b -> c
hyloAnnExpr = hylo


histoAnnExpr :: (F.CofreeF (ExprF a) ann (Cofree (F.CofreeF (ExprF a) ann) b) -> b)
             -> AnnExpr a ann -> b
histoAnnExpr = histo


deAnnProg :: AnnProgram a ann -> Program a
deAnnProg = map deAnnScDefn

deAnnScDefn :: AnnScDefn a ann -> ScDefn a
deAnnScDefn = \ case
    (name, as, ae) -> (name, as, deAnnExpr ae)

deAnnExpr :: AnnExpr a ann -> Expr a
deAnnExpr = cataAnnExpr phi where
    phi = \ case
        _ F.:< ENumF n       -> ENum n
        _ F.:< EVarF v       -> EVar v
        _ F.:< EConstrF t a  -> EConstr t a
        _ F.:< EApF e1 e2    -> EAp e1 e2
        _ F.:< ECaseF e alts -> ECase e alts
        _ F.:< ELetF r ds e  -> ELet r ds e
        _ F.:< ELamF as e    -> ELam as e

class VarRep a where
    vname :: a -> Name

instance VarRep Name where
    vname :: Name -> Name
    vname = id

instance VarRep (a, Name) where
    vname :: (a, Name) -> Name
    vname = snd

instance VarRep (Name, a) where
    vname :: (Name, a) -> Name
    vname = fst
