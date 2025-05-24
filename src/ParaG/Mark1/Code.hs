module ParaG.Mark1.Code
    ( GmCode
    , Instruction (..)
    , GmGlobalMode (..)
    )
    where

import Parser
import Language hiding (takeFirstParse)
import Utils

type GmCode = [Instruction]

data Instruction
    = Unwind
    | PushGlobal GmGlobalMode
    | PushInt Int
    | Push Int
    | Pop Int
    | Update Int
    | MkAp
    | Slide Int
    | Alloc Int
    | Eval
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | And | Or | Not
    | Cond GmCode GmCode
    | Pack Tag Arity
    | CaseJump (Assoc Tag GmCode)
    | Split Arity
    | PushBasic Int
    | MkBool
    | MkInt
    | UpdateInt Int
    | UpdateBool Int
    | Get
    | Return
    | Print
    | Par
    deriving (Eq, Show, Read)

data GmGlobalMode
    = GlobalLabel Name
    | GlobalPack Int Int
    deriving (Eq)

instance Show GmGlobalMode where
    show :: GmGlobalMode -> String
    show mode = case mode of
        GlobalLabel name -> name
        GlobalPack tag arity -> "Pack{"++show tag++","++show arity++"}"

instance Read GmGlobalMode where
    readsPrec :: Int -> ReadS GmGlobalMode
    readsPrec _ s = [(readGlobalMode s,"")]

readGlobalMode :: String -> GmGlobalMode
readGlobalMode = takeFirstParse . pGlobalMode . clex 1

pGlobalMode :: Parser GmGlobalMode
pGlobalMode =  pGlobalPack `pAlt'` pGlobalLabel

pGlobalPack :: Parser GmGlobalMode
pGlobalPack = uncurry GlobalPack <$$ pLit "Pack" <** pLit "{" <**> pTagArity <** pLit "}"

pGlobalLabel :: Parser GmGlobalMode
pGlobalLabel = GlobalLabel <$$> pSat (const True)
