module Template.Mark5.Primitive
  where

import Language
import Utils

data Primitive
    = Neg
    | Add | Sub | Mul | Div
    | Less | LessEq | Greater | GreaterEq
    | Eq | NotEq
    | PrimConstr Tag Arity
    | If
    | PrimCasePair
    | PrimCaseList
    | Abort
    | Stop
    | Print
    deriving Show

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add), ("-", Sub)
             , ("*", Mul), ("/", Div)
             , ("<", Less), ("<=", LessEq)
             , (">", Greater), (">=", GreaterEq)
             , ("==", Eq), ("/=", NotEq)
             , ("if", If)
             , ("casePair", PrimCasePair)
             , ("caseList", PrimCaseList)
             , ("abort", Abort)
             , ("stop", Stop)
             , ("print", Print)
             ]
