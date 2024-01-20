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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Gmachine.AExpr where

import Data.Type.Equality

import Nat
import List

data AExpr
    = Numb Nat
    | Plus AExpr AExpr
    | Mult AExpr AExpr
    deriving (Eq)

aInterp :: AExpr -> Nat
aInterp (Numb n) = n
aInterp (Plus e1 e2) = aInterp e1 + aInterp e2
aInterp (Mult e1 e2) = aInterp e1 * aInterp e2

data AInstruction
    = INumb Nat
    | IPlus
    | IMult
    deriving (Eq)

type Code = [AInstruction]
type Stk  = [Nat]

aCompile' :: AExpr -> Code
aCompile' (Numb n)     = [INumb n]
aCompile' (Plus e1 e2) = aCompile' e1 ++ aCompile' e2 ++ [IPlus]
aCompile' (Mult e1 e2) = aCompile' e1 ++ aCompile' e2 ++ [IMult]

{-
一般化

仕様： aCompile e c = aCompile' e ++ c

定義の導出

e : Numb n のとき

    aCompile (Numb n) c
=   { 仕様 }
    aCompile' (Numb n) ++ c
=   { aCompile' の定義 } 
    [INumb n] ++ c
=   { ++ の定義 }
    INumb n : ([] ++ c)
=   { ++ の左単位元 }
    INumb n : c
 
e : Plus e1 e2 のとき

    aCompile (Plus e1 e2) c
=   { 仕様 }
    aCompile' (Plus e1 e2) ++ c
=   { aCompile' の定義 }
    aCompile' e1 ++ (aCompile' e2 ++ [IPlus]) ++ c
=   { ++ の結合律 }
    aCompile' e1 ++ (aCompile' e2 ++ ([IPlus] ++ c))
=   { ++ の定義 }
    aCompile' e1 ++ (aCompile' e2 ++ (IPlus : ([] ++ c)))
=   { ++ の左単位元 }
    aCompile' e1 ++ (aCompile' e2 ++ IPlus n : c)
=   { 仕様 }
    aCompile e1 (aCompile e2 (IPlus n : c))

e : Mult e1 e2 のとき
    上と同様
-}

aCompile :: AExpr -> Code -> Code
aCompile (Numb n)     c = INumb n : c
aCompile (Plus e1 e2) c = aCompile e1 (aCompile e2 (IPlus : c))
aCompile (Mult e1 e2) c = aCompile e1 (aCompile e2 (IMult : c))

aEval :: Code -> [Nat] -> [Nat]
aEval []            ns             = ns
aEval (INumb n : c) ns             = aEval c (n : ns)
aEval (IPlus   : c) (n1 : n2 : ns) = aEval c (n2 + n1 : ns)
aEval (IMult   : c) (n1 : n2 : ns) = aEval c (n2 * n1 : ns)
aEval _ _ = error "aEval: invalid stack"

{-
コンパイラの正しさ

    aEval (aCompile e c) s = aEval c (aInterp e : s)

証明（等式論証）
    
    e に関する構造帰納法

    基底部 (e が Numb n の場合)

        aEval (aCompile (Numb n) c) s
    =   { aCompile の定義 }
        aEval (INumb n : c) s
    =   { aEval の定義 }
        aEval c (n : s)
    =   { aInterp の定義 }
        aEval c (aInterp (Numb n) : s)
    
    帰納部 (e が Plus e1 e2 の場合)
        aEval (aCompile (Plus e1 e2) c) s
    =   { aCompile の定義 }
        aEval (aCompile e1 (aCompile e2 (IPlus : c))) s
    =   { 帰納法の仮定より }
        aEval (aCompile e2 (IPlus : c)) (aInterp x : s) 
    =   { 帰納法の仮定 }
        aEval (IPlus : c) (aInterp e2 : aInterp e1 : s)
    =   { aEval の定義 }
        aEval c ((aInterp e1 + aInterp e2) : s)
    =   { aInterp の定義 }
        aEval c (aInterp (Plus e1 e2) : s)
    
    帰納部 (e が Mult e1 e2 の場合)
        上と同様
-}

{- 型族 -}

type family AInterp (e :: AExpr) :: Nat where
    AInterp ('Numb n)     = n
    AInterp ('Plus e1 e2) = AInterp e1 + AInterp e2
    AInterp ('Mult e1 e2) = AInterp e1 * AInterp e2

type family ACompile (e :: AExpr) (c :: Code) :: Code where
    ACompile ('Numb n)     c = 'INumb n ': c
    ACompile ('Plus e1 e2) c = ACompile e1 (ACompile e2 ('IPlus ': c))
    ACompile ('Mult e1 e2) c = ACompile e1 (ACompile e2 ('IMult ': c))

type family AEval (c :: Code) (s :: Stk) :: Stk where
    AEval '[]             s               = s
    AEval ('INumb n ': c) s               = AEval c (n         ': s)
    AEval ('IPlus   ': c) (n0 ': n1 ': s) = AEval c ((n1 + n0) ': s)
    AEval ('IMult   ': c) (n0 ': n1 ': s) = AEval c ((n1 * n0) ': s)

{- シングルトン -}

data instance Sing (e :: AExpr) where
    SNumb :: Sing (n :: Nat) -> Sing ('Numb n)
    SPlus :: Sing e1 -> Sing e2 -> Sing ('Plus e1 e2)
    SMult :: Sing e1 -> Sing e2 -> Sing ('Mult e1 e2)

data instance Sing (i :: AInstruction) where
    SINumb :: Sing (n :: Nat) -> Sing ('INumb n)
    SIPlus :: Sing 'IPlus
    SIMult :: Sing 'IMult

saInterp :: Sing (e :: AExpr) -> Sing (AInterp e)
saInterp (SNumb sn)      = sn
saInterp (SPlus se1 se2) = saInterp se1 %+ saInterp se2
saInterp (SMult se1 se2) = saInterp se1 %* saInterp se2

saCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (ACompile e c)
saCompile (SNumb sn)      sc = SINumb sn :% sc
saCompile (SPlus se1 se2) sc = saCompile se1 (saCompile se2 (SIPlus :% sc))
saCompile (SMult se1 se2) sc = saCompile se1 (saCompile se2 (SIMult :% sc))

saEval :: Sing (c :: Code) -> Sing (s :: Stk) -> Sing (AEval c s)
saEval SNil              ss                   = ss
saEval (SINumb sn :% sc) ss                   = saEval sc (sn           :% ss)
saEval (SIPlus    :% sc) (sn0 :% (sn1 :% ss)) = saEval sc ((sn1 %+ sn0) :% ss)
saEval (SIMult    :% sc) (sn0 :% (sn1 :% ss)) = saEval sc ((sn1 %* sn0) :% ss)
saEval _ _ = error "saEval: invalid stack"

{- コンパイルの正しさ（関数の型シグネチャ）と証明（関数実装） -}

validCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (s :: Stk)
             -> AEval (ACompile e c) s :~: AEval c (AInterp e ': s)
validCompile (SNumb _)       _  _ = Refl
validCompile (SPlus se1 se2) sc ss
    = case validCompile se1 (saCompile se2 (SIPlus :% sc)) ss of
        Refl -> case validCompile se2 (SIPlus :% sc) (saInterp se1 :% ss) of
            Refl -> Refl
validCompile (SMult se1 se2) sc ss
    = case validCompile se1 (saCompile se2 (SIMult :% sc)) ss of
        Refl -> case validCompile se2 (SIMult :% sc) (saInterp se1 :% ss) of
            Refl -> Refl
