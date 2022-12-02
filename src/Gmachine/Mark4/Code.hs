module Gmachine.Mark4.Code
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
    | Eval
    | Add | Sub | Mul | Div
    | Eq | Ne | Lt | Le | Gt | Ge
    | Cond GmCode GmCode
    deriving (Eq, Show)
