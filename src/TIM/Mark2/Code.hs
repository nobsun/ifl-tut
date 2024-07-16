module TIM.Mark2.Code
    where

import Data.List
import Data.Maybe
import Language
import Utils

type Code = [Instruction]

data Instruction
    = Take Int
    | Enter TimAMode
    | Push TimAMode
    | PushV ValueAMode
    | Return
    | Op Op
    | Cond Code Code
    deriving (Eq, Show)

data Op
    = Add | Sub | Mul | Div 
    | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

data TimAMode
    = Arg Int
    | Label Name
    | Code Code
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

useds :: Code -> [Int]
useds = sort . mapMaybe slot
    where
        slot :: Instruction -> Maybe Int
        slot c = case c of
            Enter (Arg n) -> Just n
            _             -> Nothing
