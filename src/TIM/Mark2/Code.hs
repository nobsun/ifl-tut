module TIM.Mark1.Code
    where

import Language
import Utils

type Code = [Instruction]

data Instruction
    = Take Int
    | Enter TimAMode
    | Push TimAMode
    deriving (Eq)

data TimAMode
    = Arg Int
    | Label String
    | Code [Instruction]
    | IntConst Int
    deriving (Eq)

type CodeStore = Assoc Name Code

codeLookup :: CodeStore -> Name -> Code
codeLookup cstore lab
    = aLookup cstore lab (error ("codeLookup: Attempt to jump to unknown label "
                               ++ show lab))
