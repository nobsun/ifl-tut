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
    | Cond [Instruction] [Instruction]
    deriving (Eq)

data Op
    = Add | Sub | Mul | Div | Neg
    | Gr | GrEq | Lt | LtEq | Eq | NotEq
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label String
    | Code [Instruction]
    | IntConst Int
    deriving (Eq)

data ValueAMode
    = FramePtr
    | IntVConst Int
    deriving (Eq, Show)

type CodeStore = Assoc Name Code

codeLookup :: CodeStore -> Name -> Code
codeLookup cstore lab
    = aLookup cstore lab (error ("codeLookup: Attempt to jump to unknown label "
                               ++ show lab))
