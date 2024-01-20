{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Singleton
    where

import Data.Kind ( Type )
import Data.Type.Equality
import Data.Void
import Unsafe.Coerce ( unsafeCoerce )

data family Sing (a :: k)

data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing
    :: SomeSing k
    -> (forall (a :: k). Sing a -> r)
    -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
    type Demote k = r | r -> k
    toSing :: Demote k -> SomeSing k
    fromSing :: Sing (a :: k) -> Demote k

class SingI (a :: k) where
    sing :: Sing a

class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)

data Decision a 
    = Proved a
    | Disproved (a -> Void)

instance (Eq (Demote k), SingKind k)
    => SDecide k where
    a %~ b = if
        | fromSing a == fromSing b -> Proved $ unsafeCoerce Refl
        | otherwise                -> Disproved $ const undefined

data Sigma (f :: k -> Type) where
    Sigma :: Sing a -> f a -> Sigma f

withSigma
    :: (forall (a :: k). Sing a -> f a -> r)
    -> Sigma f
    -> r
withSigma c (Sigma s f) = c s f

toSigma
    :: SingI a
    => f a
    -> Sigma f
toSigma fa = Sigma sing fa

fromSigma
    :: forall k (a :: k) (f :: k -> Type). (SingI a, SDecide k)
    => Sigma f
    -> Maybe (f a)
fromSigma = \ case 
    Sigma s f -> case s %~ (sing :: Sing a) of
        Proved Refl -> Just f
        Disproved _ -> Nothing

data Sigma2 (f :: j -> k -> Type) where
    Sigma2 :: Sing a -> Sing b -> f a b -> Sigma2 f

withSigma2
    :: (forall (a :: j) (b :: k). Sing a -> Sing b -> f a b -> r)
    -> Sigma2 f
    -> r
withSigma2 c (Sigma2 s t f) = c s t f

toSigma2
    :: (SingI a, SingI b)
    => f a b
    -> Sigma2 f
toSigma2 fab = Sigma2 sing sing fab

fromSigma2
    :: forall j k (a :: j) (b :: k) (f :: j -> k -> Type). (SingI a, SingI b, SDecide j, SDecide k)
    => Sigma2 f
    -> Maybe (f a b)
fromSigma2 = \ case
    Sigma2 s t f -> case s %~ (sing :: Sing a) of
        Proved Refl -> case t %~ (sing :: Sing b) of
            Proved Refl -> Just f
            Disproved _ -> Nothing
        Disproved _ -> Nothing

