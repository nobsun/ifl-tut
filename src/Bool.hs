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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bool where

import Data.Type.Equality
import Singleton

-- Bool

data instance Sing (b :: Bool) where
    SFalse :: Sing 'False
    STrue  :: Sing 'True
    
instance SingKind Bool where
    type Demote Bool = Bool
    toSing = \ case
        False -> SomeSing SFalse
        True  -> SomeSing STrue
    fromSing = \ case
        SFalse -> False
        STrue  -> True

instance SingI 'False where
    sing = SFalse

instance SingI 'True where
    sing = STrue

instance SDecide Bool where
    SFalse %~ SFalse = Proved Refl
    STrue  %~ STrue  = Proved Refl
    _      %~ _      = Disproved $ const undefined
