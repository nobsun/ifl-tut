module Gmachine.Mark3.Code
  where

import Language

type GmCode = [Instruction]

data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Update Int
    | Pop Int
    deriving (Eq, Show)
