module Gmachine.Mark5.Code
  where

import Language

type GmCode = [Instruction]

data Instruction
    = Slide Int
    | Alloc Int
    | Update Int
    | Pop Int
    | Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Eval
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | Cond GmCode GmCode
    deriving (Eq, Show)
