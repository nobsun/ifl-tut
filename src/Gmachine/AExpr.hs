{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Gmachine.AExpr where

import Data.Type.Equality
import Numeric.Natural

import Nat
import List
import Singleton

data AExpr
    = Numb Nat
    | Plus AExpr AExpr
    | Mult AExpr AExpr
    deriving (Eq)

aInterp :: AExpr -> Nat
aInterp (Numb n) = n
aInterp (Plus e1 e2) = aInterp e1 + aInterp e2
aInterp (Mult e1 e2) = aInterp e2 * aInterp e2

data AInstruction
    = INumb Nat
    | IPlus
    | IMult
    deriving (Eq)

type Code = [AInstruction]
type Stk  = [Nat]

aCompile :: AExpr -> Code -> Code
aCompile (Numb n) is = INumb n : is
aCompile (Plus e1 e2) is = aCompile e1 (aCompile e2 (IPlus : is))
aCompile (Mult e1 e2) is = aCompile e1 (aCompile e2 (IMult : is))

aEval :: Code -> [Nat] -> [Nat]
aEval []            ns             = ns
aEval (INumb n : c) ns             = ns
aEval (IPlus   : c) (n0 : n1 : ns) = (n1 + n0) : ns
aEval (IMult   : c) (n0 : n1 : ns) = (n1 * n0) : ns

type family AInterp (e :: AExpr) :: Nat where
    AInterp (Numb n)     = n
    AInterp (Plus e1 e2) = AInterp e1 + AInterp e2
    AInterp (Mult e1 e2) = AInterp e1 * AInterp e2

type family ACompile (e :: AExpr) (c :: Code) :: Code where
    ACompile (Numb n)     c = 'INumb n ': c
    ACompile (Plus e1 e2) c = ACompile e1 (ACompile e2 ('IPlus ': c))
    ACompile (Mult e1 e2) c = ACompile e1 (ACompile e2 ('IMult ': c))

type family AEval (c :: Code) (s :: Stk) :: Stk where
    AEval '[]             s               = s
    AEval ('INumb n ': c) s               = AEval c (n         ': s)
    AEval ('IPlus   ': c) (n0 ': n1 ': s) = AEval c ((n1 + n0) ': s)
    AEval ('IMult   ': c) (n0 ': n1 ': s) = AEval c ((n1 * n0) ': s)

data instance Sing (e :: AExpr) where
    SNumb :: Sing (n :: Nat) -> Sing (Numb n)
    SPlus :: Sing e1 -> Sing e2 -> Sing (Plus e1 e2)
    SMult :: Sing e1 -> Sing e2 -> Sing (Mult e1 e2)

data instance Sing (i :: AInstruction) where
    SINumb :: Sing (n :: Nat) -> Sing (INumb n)
    SIPlus :: Sing IPlus
    SIMult :: Sing IMult

saInterp :: Sing (e :: AExpr) -> Sing (AInterp e)
saInterp (SNumb sn) = sn
saInterp (SPlus se1 se2) = saInterp se1 %+ saInterp se2
saInterp (SMult se1 se2) = saInterp se1 %* saInterp se2

saCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (ACompile e c)
saCompile (SNumb sn)      sc = SCons (SINumb sn) sc
saCompile (SPlus se1 se2) sc = saCompile se1 (saCompile se2 (SCons SIPlus sc))
saCompile (SMult se1 se2) sc = saCompile se1 (saCompile se2 (SCons SIMult sc))

saEval :: Sing (c :: Code) -> Sing (s :: Stk) -> Sing (AEval c s)
saEval SNil                   ss                         = ss
saEval (SCons (SINumb sn) sc) ss                         = saEval sc (SCons sn           ss)
saEval (SCons SIPlus      sc) (SCons sn0 (SCons sn1 ss)) = saEval sc (SCons (sn1 %+ sn0) ss)
saEval (SCons SIMult      sc) (SCons sn0 (SCons sn1 ss)) = saEval sc (SCons (sn1 %* sn0) ss)

validEvalCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (s :: Stk)
                  -> AEval (ACompile e c) s :~: AEval c (AInterp e ': s)
validEvalCompile (SNumb sn)      sc ss = Refl
validEvalCompile (SPlus se1 se2) sc ss = case validEvalCompile se1 (saCompile se2 (SCons SIPlus sc)) ss of
    Refl -> case validEvalCompile se2 (SCons SIPlus sc) (SCons (saInterp se1) ss) of
        Refl -> Refl
validEvalCompile (SMult se1 se2) sc ss = case validEvalCompile se1 (saCompile se2 (SCons SIMult sc)) ss of
    Refl -> case validEvalCompile se2 (SCons SIMult sc) (SCons (saInterp se1) ss) of
        Refl -> Refl

distribEval :: Sing (c :: Code) -> Sing (d :: Code) -> Sing (s :: Stk)
            -> AEval (c ++ d) s :~: AEval d (AEval c s)
distribEval sc sd ss = case sc of
    SNil                  -> Refl
    SCons (SINumb sn) sc' -> case distribEval sc' sd (SCons sn ss) of
        Refl -> Refl
    SCons SIPlus sc'      -> case ss of
        SCons sn1 (SCons sn2 ss')
                            -> case distribEval sc' sd (SCons (sn2 %+ sn1) ss') of
            Refl -> Refl
    SCons SIMult sc'      -> case ss of
        SCons sn1 (SCons sn2 ss')
                            -> case distribEval sc' sd (SCons (sn2 %* sn1) ss') of
            Refl -> Refl
