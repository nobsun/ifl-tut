module TIM.Mark1gc.Code
    where

import Language
import Utils

type Code = [Instruction]

data Instruction
    = Take Int
    | Enter TimAMode
    | Push TimAMode
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label String
    | Code Code
    | IntConst Int
    deriving (Eq, Show)

type CodeStore = Assoc Name Code

codeLookup :: CodeStore -> Name -> Code
codeLookup cstore lab
    = aLookup cstore lab (error ("codeLookup: Attempt to jump to unknown label "
                               ++ show lab))
