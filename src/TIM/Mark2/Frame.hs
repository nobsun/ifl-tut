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

type Frame = [Closure]