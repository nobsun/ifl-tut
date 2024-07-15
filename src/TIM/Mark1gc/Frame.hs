module TIM.Mark1gc.Frame
    where

import Heap
import TIM.Mark1gc.Code

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
