module TIM.Mark6.Frame
    where
import Language
import Utils
import Heap
import TIM.Mark6.Code

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

