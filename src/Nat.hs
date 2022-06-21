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

module Nat where

import Numeric.Natural
import Data.Type.Equality
import Singleton

-- Nat
data Nat
    = Z
    | S Nat
    deriving (Eq)

toNatural :: Nat -> Natural
toNatural = \ case
    Z   -> 0
    S n -> succ (toNatural n)

fromNatural :: Natural -> Nat
fromNatural = \ case
    0 -> Z
    n -> S (fromNatural (pred n))

instance Num Nat where
    m + n = fromNatural (toNatural m + toNatural n)
    m - n = fromNatural (toNatural m - toNatural n)
    m * n = fromNatural (toNatural m * toNatural n)
    abs    = id
    signum = fromNatural . toNatural
    fromInteger = fromNatural . fromInteger 

data instance Sing (n :: Nat) where
    SZ :: Sing 'Z
    SS :: Sing (n :: Nat) -> Sing ('S n)

instance SingKind Nat where
    type Demote Nat = Nat
    toSing = \ case
        Z   -> SomeSing SZ
        S n -> withSomeSing (toSing n) $ SomeSing . SS
    fromSing = \ case
        SZ    -> Z
        SS sn -> S (fromSing sn)

instance SingI 'Z where
    sing = SZ

instance (SingI (n :: Nat)) => SingI ('S n) where
    sing = SS sing

type family (m :: Nat) + (n :: Nat) :: Nat where
    Z   + n = n
    S m + n = S (m + n)

(%+) :: Sing (m :: Nat) -> Sing (n :: Nat) -> Sing (m + n :: Nat)
SZ    %+ sn = sn
SS sm %+ sn = SS (sm %+ sn)

associativityOfPlus :: Sing (i :: Nat) -> Sing (j :: Nat) -> Sing (k :: Nat)
                    -> (i + j) + k :~: i + (j + k)
associativityOfPlus si sj sk
    = case si of
        SZ -> Refl
        SS si' -> case associativityOfPlus si' sj sk of
            Refl   -> Refl

leftUnit :: Sing (n :: Nat) -> Z + n :~: n
leftUnit sn = Refl

rightUnit :: Sing (n :: Nat) -> n + Z :~: n
rightUnit sn = case sn of
    SZ     -> Refl
    SS sn' -> case rightUnit sn' of
        Refl -> Refl

swapSucc :: Sing (m :: Nat) -> Sing (n :: Nat)
         -> S m + n :~: m + S n
swapSucc sm sn = case sm of
    SZ     -> Refl
    SS sm' -> case swapSucc sm' sn of
        Refl   -> Refl

plusOne :: Sing (m :: Nat) -> m + S Z :~: S m
plusOne sm = case sm of
    SZ -> Refl
    SS sm' -> case plusOne sm' of
        Refl   -> case swapSucc sm' (SS SZ) of
            Refl -> Refl

commutativityOfPlus :: Sing (m :: Nat) -> Sing (n :: Nat)
                    -> m + n :~: n + m
commutativityOfPlus sm sn = case sm of
    SZ -> case rightUnit sn of
        Refl -> Refl
    SS sm' -> case commutativityOfPlus sm' sn of
        Refl -> case swapSucc (SS sn) sm' of
            Refl -> Refl

type family (m :: Nat) * (n :: Nat) :: Nat where
    'Z   * n = 'Z
    'S m * n = (m * n) + n

(%*) :: Sing (m :: Nat) -> Sing (n :: Nat) -> Sing (m * n)
SZ    %* sn = SZ
SS sm %* sn = (sm %* sn) %+ sn

distribPlusOnMult :: Sing (i :: Nat) -> Sing (j :: Nat) -> Sing (k :: Nat)
                  -> (i + j) * k :~: (i * k) + (j * k)
distribPlusOnMult si sj sk = case si of
    SZ     -> Refl
    SS si' -> case distribPlusOnMult si' sj sk of
        Refl   -> case associativityOfPlus (si' %* sk) (sj %* sk) sk of
            Refl   -> case commutativityOfPlus (sj %* sk) sk of
                Refl   -> case associativityOfPlus (si' %* sk) sk (sj %* sk) of
                    Refl   -> Refl 

distribMultOnPlus :: Sing (i :: Nat) -> Sing (j :: Nat) -> Sing (k :: Nat)
                  -> i * (j + k) :~: (i * j) + (i * k)
distribMultOnPlus si sj sk = case si of
    SZ     -> Refl
    SS si' -> case distribMultOnPlus si' sj sk of
        Refl   -> case associativityOfPlus ((si' %* sj) %+ (si' %* sk)) sj sk of
            Refl   -> case associativityOfPlus (si' %* sj) (si' %* sk) sj of
                Refl   -> case commutativityOfPlus (si' %* sk) sj of
                    Refl   -> case associativityOfPlus (si' %* sj) sj (si' %* sk) of
                        Refl   -> case associativityOfPlus (si %* sj) (si' %* sk) sk of
                            Refl   -> Refl

associativityOfMult :: Sing (i :: Nat) -> Sing (j :: Nat) -> Sing (k :: Nat)
                    -> (i * j) * k :~: i * (j * k)
associativityOfMult si sj sk = case si of
    SZ     -> Refl
    SS si' -> case distribPlusOnMult (si' %* sj) sj sk of
        Refl   -> case associativityOfMult si' sj sk of
            Refl   -> Refl

leftZero :: Sing (m :: Nat) -> Z * m :~: Z
leftZero sm = Refl

rightZero :: Sing (m :: Nat) -> m * Z :~: Z
rightZero sm = case sm of
    SZ -> Refl
    SS sm' -> case rightZero sm' of
        Refl   -> Refl

leftUnitMult :: Sing (m :: Nat) -> S Z * m :~: m
leftUnitMult sm = Refl

rightUnitMult :: Sing (m :: Nat) -> m * S Z :~: m
rightUnitMult sm = case sm of
    SZ     -> Refl
    SS sm' -> case rightUnitMult sm' of
        Refl   -> case plusOne sm' of
            Refl   -> Refl
 
commutativityOfMult :: Sing (m :: Nat) -> Sing (n :: Nat)
                    -> m * n :~: n * m
commutativityOfMult sm sn = case sm of
    SZ     -> case rightZero sn of
        Refl   -> Refl 
    SS sm' -> case commutativityOfMult sm' sn of
        Refl   -> case rightUnitMult sn of
            Refl   -> case distribMultOnPlus sn sm' (SS SZ) of
                Refl   -> case plusOne sm' of
                    Refl   -> Refl
