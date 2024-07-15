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

selectClosures :: [Int] -> [Closure] -> [Closure]
selectClosures slots cs = select slots (zip [1 ..] cs)
    where
        select [] _ = []
        select _ [] = []
        select iis@(i:is) jcjcs@((j, c) : jcs) = case compare i j of
            LT -> select is jcjcs
            EQ -> c : select is jcs
            GT -> select iis jcs
