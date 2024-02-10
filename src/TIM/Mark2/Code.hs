module TIM.Mark2.Code
    where

import Language
import Utils

type Code = [Instruction]

data Instruction
    = Take Int
    | Push TimAMode
    | PushV ValueAMode
    | Enter TimAMode
    | Return
    | Op Op
    | Cond Code Code
    deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label String
    | Code [Instruction]
    | IntConst Int
    deriving (Eq, Show)

data ValueAMode
    = FramePtr
    | IntVConst Int
    deriving (Eq, Show)

type CodeStore = Assoc Name Code

codeLookup :: CodeStore -> Name -> Code
codeLookup cstore lab
    = aLookup cstore lab (error ("codeLookup: Attempt to jump to unknown label "
                               ++ show lab))
