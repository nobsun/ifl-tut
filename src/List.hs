{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module List where

import Data.Type.Equality
import Nat
import Singleton

-- List
data instance Sing (a :: [k]) where
    SNil :: Sing '[]
    (:%) :: Sing (h :: k)
         -> Sing (t :: [k])
         -> Sing (h ': t)

infixr 5 :%

instance (k ~ Demote k, SingKind k) => SingKind [k] where
    type Demote [k] = [k]
    toSing = \ case
        []  -> SomeSing SNil
        h:t -> withSomeSing (toSing h) $ \ sh ->
                   withSomeSing (toSing t) $ \ st -> SomeSing $ sh :% st
    fromSing = \ case 
        SNil     -> []
        sh :% st -> fromSing sh : fromSing st

instance (SingI (h :: k), SingI (t :: [k])) => SingI (h ': t) where
    sing = sing :% sing

--
type family Head (α :: [k]) :: k where
    Head (h ': t) = h

sHead :: Sing (α :: [k]) -> SomeSing k
sHead = \ case
    h :% _ -> SomeSing h 

type family Tail (α :: [k]) :: [k] where
    Tail (h ': t) = t

sTail :: Sing (α :: [k]) -> SomeSing [k]
sTail = \ case
    _ :% t -> SomeSing t

type family Last (α :: [k]) :: k where
    Last (h ': '[]) = h
    Last (h ': t)   = Last t

sLast :: Sing (α :: [k]) -> SomeSing k
sLast = \ case
    h :% SNil -> SomeSing h
    h :% t    -> sLast t

type family Init (α :: [k]) :: [k] where
    Init (h ': '[]) = '[]
    Init (h ': t)   = h ': Init t

sInit :: Sing (α :: [k]) -> SomeSing [k]
sInit = \ case
    h :% SNil -> SomeSing SNil
    h :% t    -> withSomeSing (sInit t) $ \ st -> SomeSing (h :% st)

type family Take (n :: Nat) (α :: [k]) :: [k] where
    Take Z α            = '[]
    Take (S n) '[]      = '[]
    Take (S n) (h ': t) = h ': Take n t

type family Drop (n :: Nat) (α :: [k]) :: [k] where
    Drop Z     α        = α
    Drop (S n) '[]      = '[]
    Drop (S n) (h ': t) = Drop n t

sTake :: Sing (n :: Nat) -> Sing (α :: [k]) -> SomeSing [k]
sTake SZ      _          = SomeSing SNil
sTake (SS sn) SNil       = SomeSing SNil
sTake (SS sn) (sh :% st) = withSomeSing (sTake sn st) $ \ st' -> SomeSing (sh :% st')

sDrop :: Sing (n :: Nat) -> Sing (α :: [k]) -> SomeSing [k]
sDrop SZ sα             = SomeSing sα
sDrop (SS sn) SNil      = SomeSing SNil
sDrop (SS sn) (_ :% st) = sDrop sn st

type family Length (α :: [k]) :: Nat where
    Length '[]      = Z
    Length (h ': t) = S (Length t)

sLength :: Sing (α :: [k]) -> SomeSing Nat
sLength = \ case
    SNil   -> SomeSing SZ
    _ :% t -> withSomeSing (sLength t) (SomeSing . SS)

type family (α :: [k]) ++ (β :: [k]) :: [k] where
    '[]      ++ β = β
    (a ': α) ++ β = a ': (α ++ β)

infixr 5 ++

(%++) :: Sing (α :: [k]) -> Sing (β :: [k]) -> Sing (α ++ β)
SNil     %++ b = b
(h :% a) %++ b = h :% (a %++ b)

infixr 5 %++

distributeLength :: Sing (α :: [k]) -> Sing (β :: [k])
                   -> Length (α ++ β) :~: Length α + Length β
distributeLength a b = case a of
    SNil   -> Refl
    _ :% t -> case distributeLength t b of
        Refl      -> Refl

associativityOfJuxtaposition
    :: Sing α -> Sing β -> Sing γ
    -> (α ++ β) ++ γ :~: α ++ (β ++ γ)
associativityOfJuxtaposition a b c
    = case a of
        SNil   -> Refl
        h :% t -> case associativityOfJuxtaposition t b c of
            Refl      -> Refl

leftCancelationOfJuxtaposition
    :: Sing α -> Sing β -> Sing γ
    -> α ++ β :~: α ++ γ
    -> β :~: γ
leftCancelationOfJuxtaposition a b c Refl
    = case a of
        SNil      -> Refl
        _ :% t -> case leftCancelationOfJuxtaposition t b c Refl of
            Refl      -> Refl

newtype Compat α β α' β' = Compat (α ++ α' :~: β ++ β')
type Compatible α β = Sigma2 (Compat α β)
