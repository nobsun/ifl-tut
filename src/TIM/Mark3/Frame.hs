module TIM.Mark3.Frame
    where

import Heap
import TIM.Mark3.Code

data FramePtr
    = FrameAddr Addr  -- ^ The address of frame
    | FrameInt Int    -- ^ An integer value
    | FrameNull       -- ^ uninitialised
    deriving (Eq, Show)

type Closure = (CCode, FramePtr)

type Frame = [Closure]
