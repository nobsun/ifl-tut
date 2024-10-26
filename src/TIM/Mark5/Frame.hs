module TIM.Mark5.Frame
    where

import Heap
import TIM.Mark5.Code

data FramePtr
    = FrameAddr Addr  -- ^ The address of frame
    | FrameInt Int    -- ^ An integer value
    | FrameNull       -- ^ uninitialised
    deriving (Eq, Show)

type Closure = (CCode, FramePtr)

data Frame
    = Frame   { closures :: [Closure] }
    | Forward { forward  :: Addr }
    deriving (Eq, Show)
