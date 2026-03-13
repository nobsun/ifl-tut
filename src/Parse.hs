-- # Parse
module Parse
    where

import Data.Char
import Data.Maybe

import Language
import Bop
import ParserCombinator

clex :: Loc -> String -> [Token]
clex i ('-' : '-' : cs) = clex i (dropWhile ('\n' /=) cs)
clex i ('-' : '>' : cs) = (i, "→") : clex i cs
clex i (c1 : c2 : cs)
  | isBop [c1,c2] = (i,[c1,c2]) : clex i cs
clex i ('\n' : cs) = clex (succ i) cs
clex i (c : cs)
  | isDigit c = case span isDigit cs of 
      (ds, rs) -> (i, c : ds) : clex i rs
  | isAlpha c = case span isIdChar cs of
      (vs, rs) -> (i, c : vs) : clex i rs
  | isSpace c = clex i cs
  | otherwise = (i,[c]) : clex i cs 
clex _ "" = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_' || c == '\''

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram

parse :: String -> CoreProgram
parse = syntax . clex 1

parseSc :: String -> CoreScDefn
parseSc = takeFirstParse . pSc . clex 1

parseExpr :: String -> CoreExpr
parseExpr = takeFirstParse . pExpr . clex 1

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

{- コア言語の構文解析 -}
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pVarStr :: Parser String
pVarStr = pVar keywords

pSc :: Parser CoreScDefn
pSc = mkSc <$$> pVarStr <**> pMunch pVarStr <** pLit "=" <**> pExpr

mkSc :: Name -> [Name] -> CoreExpr -> (Name, [Name], CoreExpr)
mkSc = (,,)

{- コア式の構文解析器 -}
pExpr :: Parser CoreExpr
pExpr =  pELet `pAlt` (pECase `pAlt` (pELam `pAlt` (pAexpr `pAlt` pExpr1)))

pELet :: Parser CoreExpr
pELet = ELet <$$> pIsRec  <**> pDefns <** pLit "in" <**> pExpr

pIsRec :: Parser IsRec
pIsRec = pLit "let" **> pEmpty nonRecursive
  `pAlt` pLit "letrec" **> pEmpty recursive

pDefns :: Parser [(Name, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn :: Parser (Name, CoreExpr)
pDefn = (,) <$$> pVarStr <** pLit "=" <**> pExpr

pECase :: Parser CoreExpr
pECase = ECase <$$ pLit "case" <**> pExpr <** pLit "of" <**> pAlters

pAlters :: Parser [CoreAlt]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")

pAlter :: Parser CoreAlt
pAlter = (,,) <$$> pTag <**> pMunch pVarStr <** pLit "->" <**> pExpr

pTag :: Parser Tag
pTag = pLit "<" **> pNum <** pLit ">"

pELam :: Parser CoreExpr
pELam = ELam <$$ pLit "\\" <**> pMunch1 pVarStr <** pLit "->" <**> pExpr

pAexpr :: Parser CoreExpr
pAexpr = pEVar `pAlt` pENum `pAlt` pEConstr
  `pAlt` pParen pExpr

pParen :: Parser CoreExpr -> Parser CoreExpr
pParen p = pLit "(" **> p <** pLit ")"

pEVar :: Parser CoreExpr
pEVar = EVar <$$> pVarStr

pENum :: Parser CoreExpr
pENum = ENum <$$> pNum

pEConstr :: Parser CoreExpr
pEConstr = uncurry EConstr <$$ pLit "Pack" <** pLit "{" <**> pTagArity <** pLit "}"

pTagArity :: Parser (Tag, Arity)
pTagArity = (,) <$$> pNum <** pLit "," <**> pNum

pExpr1 :: Parser CoreExpr
pExpr1 = assembleOp <$$> pExpr2 <**> pExpr1c

data PartialExpr
  = NoOp
  | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e pe = case pe of
  NoOp          -> e
  FoundOp op e' -> EAp (EAp (EVar op) e) e'

pExpr1c :: Parser PartialExpr
pExpr1c = FoundOp <$$> pLit "||" <**> pExpr1
   `pAlt` pEmpty NoOp

pExpr2 :: Parser CoreExpr
pExpr2 = assembleOp <$$> pExpr3 <**> pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = FoundOp <$$> pLit "&&" <**> pExpr2
   `pAlt` pEmpty NoOp

pExpr3 :: Parser CoreExpr
pExpr3 = assembleOp <$$> pExpr4 <**> pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = FoundOp <$$> pRelop <**> pExpr4
   `pAlt` pEmpty NoOp

pRelop :: Parser String
pRelop = pSat (`elem` relops)

relops :: [String]
relops = ["<", "<=", "==", "/=", ">=", ">"]

pExpr4 :: Parser CoreExpr
pExpr4 = assembleOp <$$> pExpr5 <**> pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = FoundOp <$$> pLit "+" <**> pExpr4
  `pAlt`  FoundOp <$$> pLit "-" <**> pExpr5
  `pAlt`  pEmpty NoOp

pExpr5 :: Parser CoreExpr
pExpr5 = assembleOp <$$> pExpr6 <**> pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = FoundOp <$$> pLit "*" <**> pExpr5
  `pAlt`  FoundOp <$$> pLit "/" <**> pExpr6
  `pAlt`  pEmpty NoOp

pExpr6 :: Parser CoreExpr
pExpr6 = foldl1 EAp <$$> pMunch1 pAexpr
