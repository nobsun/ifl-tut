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
    | Slide Int
    | Update Int
    | Pop Int
    | Alloc Int
    deriving (Eq, Show)
