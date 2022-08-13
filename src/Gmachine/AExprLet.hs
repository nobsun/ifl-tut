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

module Gmachine.AExprLet where

import Data.Type.Equality
import Unsafe.Coerce ( unsafeCoerce )

import Bool
import Nat
import List
import Singleton

type Name = Nat

data AExpr
    = Numb Nat
    | Plus AExpr AExpr
    | Mult AExpr AExpr
    | Var Name
    | Let Name AExpr AExpr
    deriving (Eq)

aInterp :: AExpr -> Nat
aInterp (Numb n)     = n
aInterp (Plus e1 e2) = aInterp e1 + aInterp e2
aInterp (Mult e1 e2) = aInterp e1 * aInterp e2
aInterp (Var _)      = aInterp (Numb 0)
aInterp (Let x e b)  = aInterp (substitute b x e)

substitute :: AExpr -> Name -> AExpr -> AExpr
substitute s x t = case s of
    Numb _          -> s
    Plus e1 e2      -> Plus (substitute e1 x t) (substitute e2 x t)
    Mult e1 e2      -> Mult (substitute e1 x t) (substitute e2 x t)
    Var y           -> if x == y then t else Var y
    Let y e b       -> Let y (substitute e x t) (substitute b x t)

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
aCompile' (Var _)      = aCompile' (Numb 0)
aCompile' _ = error "aComile': cannot compile"

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
    aCompile' e1 ++ (aComile' e2 ++ [IPlus]) ++ c
=   { ++ の結合律 }
    aCompile' e1 ++ (aCompile' e2 ++ ([IPlus] ++ c))
=   { ++ の定義 }
    aCompile' e1 ++ (aCompile' e2 ++ (IPlus : ([] ++ c)))
=   { ++ の左単位元 }
    aCompile' e1 ++ (aCompile' e2 ++ IPlus n : c)
=   { 仕様 }
    aCompile e1 (aComile e2 (IPlus n : c))

e : Mult e1 e2 のとき
    上と同様
-}

aCompile :: AExpr -> Code -> Code
aCompile (Numb n)     c = INumb n : c
aCompile (Plus e1 e2) c = aCompile e1 (aCompile e2 (IPlus : c))
aCompile (Mult e1 e2) c = aCompile e1 (aCompile e2 (IMult : c))
aCompile (Var _)      c = aCompile (Numb 0) c
aCompile (Let x e b)  c = aCompile (substitute b x e) c

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
    AInterp ('Var x)      = AInterp ('Numb 'Z)
    AInterp ('Let x e b)  = AInterp (Substitute b x e)

type family Substitute (s :: AExpr) (x :: Name) (t :: AExpr) :: AExpr where
    Substitute ('Numb n)     x t = 'Numb n
    Substitute ('Plus e1 e2) x t = 'Plus (Substitute e1 x t) (Substitute e2 x t)
    Substitute ('Mult e1 e2) x t = 'Mult (Substitute e1 x t) (Substitute e2 x t)
    Substitute ('Var x)      x t = t
    Substitute ('Var x')     x t = 'Var x'
    Substitute ('Let y e b)  x t = 'Let y (Substitute e x t) (Substitute b x t)

type family ACompile (e :: AExpr) (c :: Code) :: Code where
    ACompile ('Numb n)     c = 'INumb n ': c
    ACompile ('Plus e1 e2) c = ACompile e1 (ACompile e2 ('IPlus ': c))
    ACompile ('Mult e1 e2) c = ACompile e1 (ACompile e2 ('IMult ': c))
    ACompile ('Var x)      c = ACompile ('Numb 'Z) c
    ACompile ('Let x e b)  c = ACompile (Substitute b x e) c

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
    SVar  :: Sing (x :: Name) -> Sing ('Var (x :: Name))
    SLet  :: Sing (x :: Name) -> Sing e -> Sing b -> Sing ('Let x e b)

instance SingKind AExpr where
    type Demote AExpr = AExpr
    toSing = \ case
        Numb n     -> withSomeSing (toSing n) (SomeSing . SNumb)
        Plus e1 e2 -> withSomeSing (toSing e1) $ \ se1 ->
            withSomeSing (toSing e2) $ \ se2 ->
                SomeSing (SPlus se1 se2)
        Mult e1 e2 -> withSomeSing (toSing e1) $ \ se1 ->
            withSomeSing (toSing e2) $ \ se2 ->
                SomeSing (SMult se1 se2)
        Var n      -> withSomeSing (toSing n) (SomeSing . SVar)
        Let x e b  -> withSomeSing (toSing x) $ \ sx ->
            withSomeSing (toSing e) $ \ se ->
                withSomeSing (toSing b) $ \ sb -> SomeSing (SLet sx se sb)
    fromSing = \ case
        SNumb sn -> Numb (fromSing sn)
        SPlus se1 se2 -> Plus (fromSing se1) (fromSing se2)
        SMult se1 se2 -> Mult (fromSing se1) (fromSing se2)
        SVar sx -> Var (fromSing sx)
        SLet sx se sb -> Let (fromSing sx) (fromSing se) (fromSing sb)

instance (SingI (n :: Nat)) => SingI ('Numb n) where
    sing = SNumb sing

instance (SingI (e1 :: AExpr), SingI (e2 :: AExpr)) => SingI ('Plus e1 e2) where
    sing = SPlus sing sing

instance (SingI (e1 :: AExpr), SingI (e2 :: AExpr)) => SingI ('Mult e1 e2) where
    sing = SMult sing sing

instance (SingI (x :: Nat)) => SingI ('Var x) where
    sing = SVar sing

instance (SingI (x :: Nat), SingI (e :: AExpr), SingI (b :: AExpr)) => SingI ('Let x e b) where
    sing = SLet sing sing sing

data instance Sing (i :: AInstruction) where
    SINumb :: Sing (n :: Nat) -> Sing ('INumb n)
    SIPlus :: Sing 'IPlus
    SIMult :: Sing 'IMult

saInterp :: Sing (e :: AExpr) -> Sing (AInterp e)
saInterp (SNumb sn)      = sn
saInterp (SPlus se1 se2) = saInterp se1 %+ saInterp se2
saInterp (SMult se1 se2) = saInterp se1 %* saInterp se2
saInterp (SLet sx se sb) = saInterp (saSubstitute sb sx se)
saInterp (SVar _)        = error "saInterp: unbound variable"

saSubstitute :: Sing (s :: AExpr) -> Sing (x :: Name) -> Sing (t :: AExpr) -> Sing (Substitute s x t)
saSubstitute ss sx st = case ss of
    SNumb _       -> ss
    SPlus se1 se2 -> SPlus (saSubstitute se1 sx st) (saSubstitute se2 sx st)
    SMult se1 se2 -> SMult (saSubstitute se1 sx st) (saSubstitute se2 sx st)
    SVar sy -> case SVar sx %~ SVar sy of
        Proved Refl -> st
        Disproved _ -> unsafeCoerce ss 
    SLet sy se sb -> SLet sy (saSubstitute se sx st) (saSubstitute sb sx st) 

eq :: Sing (m :: Name) -> Sing (n :: Name) -> m :~: n -> Sing ('Var m) :~: Sing ('Var n)
eq _ _ Refl = Refl

saCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (ACompile e c)
saCompile (SNumb sn)      sc = SINumb sn :% sc
saCompile (SPlus se1 se2) sc = saCompile se1 (saCompile se2 (SIPlus :% sc))
saCompile (SMult se1 se2) sc = saCompile se1 (saCompile se2 (SIMult :% sc))
saCompile (SVar _)        sc = saCompile (SNumb SZ) sc
saCompile (SLet sx se sb) sc = saCompile (saSubstitute sb sx se) sc

saEval :: Sing (c :: Code) -> Sing (s :: Stk) -> Sing (AEval c s)
saEval SNil              ss                   = ss
saEval (SINumb sn :% sc) ss                   = saEval sc (sn           :% ss)
saEval (SIPlus    :% sc) (sn0 :% (sn1 :% ss)) = saEval sc ((sn1 %+ sn0) :% ss)
saEval (SIMult    :% sc) (sn0 :% (sn1 :% ss)) = saEval sc ((sn1 %* sn0) :% ss)
saEval _ _ = error "saEval: invalid stack"

{- コンパイルの正しさ（関数の型シグネチャ）と証明（関数実装） -}

validCompile :: Sing (e :: AExpr) -> Sing (c :: Code) -> Sing (s :: Stk)
             -> AEval (ACompile e c) s :~: AEval c (AInterp e ': s)
validCompile (SNumb _)       _  _  = Refl
validCompile (SPlus se1 se2) sc ss
    = case validCompile se1 (saCompile se2 (SIPlus :% sc)) ss of
        Refl -> case validCompile se2 (SIPlus :% sc) (saInterp se1 :% ss) of
            Refl -> Refl
validCompile (SMult se1 se2) sc ss
    = case validCompile se1 (saCompile se2 (SIMult :% sc)) ss of
        Refl -> case validCompile se2 (SIMult :% sc) (saInterp se1 :% ss) of
            Refl -> Refl
validCompile (SVar _) sc ss = validCompile (SNumb SZ) sc ss
validCompile (SLet sx se sb) sc ss
    = case validCompile (saSubstitute sb sx se) sc ss of
        Refl -> Refl
