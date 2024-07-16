module TIM.Mark2.Frame
    where

import Heap
import TIM.Mark2.Code

data FramePtr
    = FrameAddr Addr  -- ^ The address of frame
    | FrameInt Int    -- ^ An integer value
    | FrameNull       -- ^ uninitialised
    deriving (Eq, Show)

type Closure = (Code, FramePtr)

data Frame
    = Frame [Closure]
    | Forward Addr
    deriving (Eq, Show)

forwardAddr :: Frame -> Addr
forwardAddr f = case f of
    Forward addr -> addr
    _            -> error "forwardAddr: Not evacuated"

closures :: Frame -> [Closure]
closures f = case f of
    Frame cs -> cs
    _        -> error "closures: Already evacuated"
