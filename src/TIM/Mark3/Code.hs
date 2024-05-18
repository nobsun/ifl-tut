module TIM.Mark3.Code
    where

import Language
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
    | Enter TimAMode
    | Return
    | Op Op
    | Cond CCode CCode
    deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label String
    | Code CCode
    | IntConst Int
    deriving (Eq, Show)

data ValueAMode
    = FramePtr
    | IntVConst Int
    deriving (Eq, Show)

type CodeStore = Assoc Name CCode

codeLookup :: CodeStore -> Name -> CCode
codeLookup cstore lab
    = aLookup cstore lab (error ("codeLookup: Attempt to jump to unknown label "
                               ++ show lab))
