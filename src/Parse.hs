-- # Parse
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Parse
    where

import Data.Char
import Data.Maybe

import Utils
import Language
import Bop
import Lexeme
import Lex
import ParserCombinator
import Data.Set qualified as S

{- CoreProgram の構文解析 -}

parse :: String -> CoreProgram
parse = takeFirstParse . pProgram.parser. clex

parseSc :: String -> CoreScDefn
parseSc = takeFirstParse . pSc.parser . clex

pProgram :: Parser CoreProgram
pProgram = pMunch1WithSep pSc (pLit LComm)

pSc :: Parser CoreScDefn
pSc = (,,) <$> pVarStr <*> pMunch pVarStr <* pLit (LBop "=") <*> pExpr

pVarStr :: Parser Name
pVarStr = fromLexeme <$> pSat p where
    p = \ case
        LIde _ -> True
        _      -> False

{- CoreExpr の構文解析 -}
parseExpr :: String -> CoreExpr
parseExpr = takeFirstParse . pExpr.parser . clex

pExpr :: Parser CoreExpr
pExpr = pELet <++ pECase <++ pELam <++ pExpr0

{- ELet -}
pELet :: Parser CoreExpr
pELet = ELet <$> pIsRec <*> pDefns <* pLit (LRsv "in") <*> pExpr

pIsRec :: Parser IsRec
pIsRec = (pLit (LRsv "letrec") *> pure recursive)
     <++ (pLit (LRsv "let") *> pure nonRecursive)

pDefns :: Parser (Assoc Name CoreExpr)
pDefns = pMunchWithSep pDefn (pLit LComm)

pDefn :: Parser (Name, CoreExpr)
pDefn = (,) <$> pVarStr <* pLit (LBop "=") <*> pExpr

{- ECase -}
pECase :: Parser CoreExpr
pECase = ECase <$ pLit (LRsv "case") <*> pExpr <*> pAlts

pAlts :: Parser [CoreAlt]
pAlts = pMunch1WithSep pAlt (pLit LComm)

pAlt :: Parser CoreAlt
pAlt = (,,) <$> pTag <*> pMunch pVarStr <* pLit LRarr <*> pExpr

pTag :: Parser Int
pTag = pLit (LBop "<") *> pNum <* pLit (LBop ">")

pNum :: Parser Int
pNum = read @Int . fromLexeme <$> pSat p where
    p = \ case
        LNum _ -> True
        _      -> False

{- ELam -}
pELam :: Parser CoreExpr
pELam = ELam <$ pLit LLambda <*> pMunch pVarStr <* pLit LRarr <*> pExpr

{- Atomic Epression -}
pAExpr :: Parser CoreExpr
pAExpr = pSection <++ pParen pExpr <++ pEVar <++ pENum <++ pEConstr

pParen :: Parser CoreExpr -> Parser CoreExpr
pParen p = pLit LOpar *> p <* pLit LCpar

pSection :: Parser CoreExpr
pSection = pSecB <++ pSecL <++ pSecR

pSecB :: Parser CoreExpr
pSecB = pParen (lamB <$> pBop) where
    lamB bop = ELam ["_x","_y"] (EAp (EAp bop (EVar "_x")) (EVar "_y"))

pSecL :: Parser CoreExpr
pSecL = pParen (lamL <$> pExpr <*> pBop) where
    lamL e bop = ELam ["_y"] (EAp (EAp bop e) (EVar "_y"))

pSecR :: Parser CoreExpr
pSecR = pParen (lamR <$> pBop <*> pExpr) where
    lamR bop e = ELam ["_x"] (EAp (EAp bop (EVar "_x")) e)

pBop :: Parser CoreExpr
pBop = EVar . fromLexeme <$> pSat p where
    p = \ case
        LBop _ -> True
        _      -> False

pBop' :: Name -> Parser CoreExpr
pBop' bop = EVar <$> pLit (LBop bop)

pEVar :: Parser CoreExpr
pEVar = EVar <$> pVarStr

pENum :: Parser CoreExpr
pENum = ENum <$> pNum

pEConstr :: Parser CoreExpr
pEConstr = uncurry EConstr <$ pLit (LRsv "Pack")
        <* pLit LObrc <*> pTagArity <* pLit LCbrc

pTagArity :: Parser (Int, Int)
pTagArity = (,) <$> pNum <* pLit LComm <*> pNum

{- Expr0 -}

pExpr0 :: Parser CoreExpr
pExpr0 = pExpr1 `pChainr1` pExpr0cR
     <++ pExpr1 `pChainl1` pExpr0cL

pExpr0cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr0cR = pExprC (pBop' "$!" <++ pBop' "$")

pExpr0cL :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr0cL = pExprC (pBop' "`seq`")

pExprC :: Parser CoreExpr -> Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExprC bop = (EAp .) . EAp <$> bop

pExpr1 :: Parser CoreExpr
pExpr1 = pExpr2 `pChainl1` pExpr1cL

pExpr1cL :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr1cL = pExprC (pBop' ">>=" <++ pBop' ">>")

pExpr2 :: Parser CoreExpr
pExpr2 = pExpr3 `pChainr1` pExpr2cR

pExpr2cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr2cR = pExprC (pBop' "||")

pExpr3 :: Parser CoreExpr
pExpr3 = pExpr4 `pChainr1` pExpr3cR

pExpr3cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr3cR = pExprC (pBop' "&&")

pExpr4 :: Parser CoreExpr
pExpr4 = (rel <$> pExpr5 <*> pRel <*> pExpr5) <++ pExpr5 where
    rel e1 o e2 = EAp (EAp o e1) e2

pRel :: Parser CoreExpr
pRel = pChoice' 
     [ pBop' o | o <- ["==","/=","<=","<",">=",">","`notElem`", "`elem`"] ]

pExpr5 :: Parser CoreExpr
pExpr5 = pExpr6 `pChainr1` pExpr5cR

pExpr5cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr5cR = pExprC (pBop' ":" <++ pBop' "++")

pExpr6 :: Parser CoreExpr
pExpr6 = pExpr7 `pChainl1` pExpr6cL

pExpr6cL :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr6cL = pExprC (pBop' "+" <++ pBop' "-")

pExpr7 :: Parser CoreExpr
pExpr7 = pExpr8 `pChainl1` pExpr7cL

pExpr7cL :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr7cL = pExprC pMulDiv

pMulDiv :: Parser CoreExpr
pMulDiv = pChoice' [ pBop' o
                   | o <- ["*","/","`div`","`mod`","`quot`","`rem`"] ]

pExpr8 :: Parser CoreExpr
pExpr8 = pExpr9 `pChainr1` pExpr8cR

pExpr8cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr8cR = pExprC (pChoice' [pBop' o | o <- ["^^","^","**"] ])

pExpr9 :: Parser CoreExpr
pExpr9 = pExprA `pChainr1` pExpr9cR

pExpr9cR :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
pExpr9cR = pExprC (pBop' ".")

pExprA :: Parser CoreExpr
pExprA = foldl1 EAp <$> pMunch1 pAExpr
