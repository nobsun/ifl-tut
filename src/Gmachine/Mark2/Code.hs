module Gmachine.Mark2.Code
  where

import Language

type GmCode = [Instruction]

data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Slide Int
    deriving (Eq, Show)
