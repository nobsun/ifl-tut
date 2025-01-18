module TIM.Mark6.Code
    where

import Language
import Heap
import Utils

type Code = [Instruction]
data CCode
    = CCode
    { slots :: [Int]
    , code  :: Code
    }
    deriving (Eq, Show)

data Instruction
    = Take Int Int
    | Move Int TimAMode
    | Push TimAMode
    | PushV ValueAMode
    | PushMarker Int
    | UpdateMarkers Int
    | Enter TimAMode
    | Return
    | Op Op
    | Switch [(Int, CCode)]
    | ReturnConstr Int
    | Print
    deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label Name Int
    | Code CCode
    | IntConst Int
    | Data Int
    deriving (Eq, Show)

data ValueAMode
    = FramePtr
    | IntVConst Int
    deriving (Eq, Show)


