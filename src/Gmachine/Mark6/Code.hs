module Gmachine.Mark6.Code
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
    | Add | Sub | Mul | Div
    | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | Cond GmCode GmCode
    | Pack Int Int
    | Casejump [(Tag, GmCode)]
    | Split Arity
    | Print
    deriving (Eq, Show)
