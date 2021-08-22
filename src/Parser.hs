{- |
module:       Parser
copyright:    (c) Nobuo Yamashita 2021
license:      BSD-3
maintainer:   nobsun@sampou.org
stability:    experimental
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser
  ( -- * パーザ
    Loc
  , Token
  , tokLoc
  , tokStr
  , Parser
  , pLit
  , pSat
  , pNum
  , pAlt
  , pAlt'
  , (<+)
  , pEmpty
  , pApply
  , pThen, pThen3, pThen4
  , pZeroOrMore, pOneOrMore
  , pMunch, pMunch1
  , pOneOrMoreWithSep
  , (<$$>), (<$$), (<**>), (<**), (**>)
  ) where

import Data.Char ( isAlpha, isDigit )

{- 構文解析用の基本的ツール -}

type Loc = Int
type Token = (Loc, String)

tokLoc :: Token -> Loc
tokLoc = fst

tokStr :: Token -> String
tokStr = snd

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (s ==)

pSat :: (String -> Bool) -> Parser String
pSat p = \ case
  (_, s) : toks
    | p s       -> [(s,toks)]
  _             -> []

pVar :: Parser String
pVar = pSat ((&&) . isAlpha . head <*> (`notElem` keywords))

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pNum :: Parser Int
pNum = read <$$> pSat (all isDigit)

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pAlt' :: Parser a -> Parser a -> Parser a
pAlt' p1 p2 toks = p1 toks <+ p2 toks

infixr 3 `pAlt`, `pAlt'`

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ ys = xs

infixr 5 <+

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1 ]

pThen3 :: (a -> b -> c -> d) 
       -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks
                                , (v2, toks2) <- p2 toks1
                                , (v3, toks3) <- p3 toks2 ]

pThen4 :: (a -> b -> c -> d -> e) 
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks
                                   , (v2, toks2) <- p2 toks1
                                   , (v3, toks3) <- p3 toks2
                                   , (v4, toks4) <- p4 toks3 ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f x, toks') | (x, toks') <- p toks]

(<$$>) :: (a -> b) -> (Parser a -> Parser b)
(<$$>) = flip pApply

(<$$) :: a -> Parser b -> Parser a
(x <$$ p) toks = [ (x, toks') | (_, toks') <- p toks ]

(<**>) :: Parser (a -> b) -> (Parser a -> Parser b)
(pf <**> px) toks = [ (f x, toks2) | (f, toks1) <- pf toks
                                   , (x, toks2) <- px toks1 ]

(<**) :: Parser a -> Parser b -> Parser a
p1 <** p2 = const <$$> p1 <**> p2

(**>) :: Parser a -> Parser b -> Parser b
p1 **> p2 = const id <$$> p1 <**> p2

{-
pThen  combine p1 p2 = combine <$$> p1 <**> p2
pThen3 combine p1 p2 p3 = combine <$$> p1 <**> p2 <**> p3
pThen4 combine p1 p2 p3 p4 = combine <$$> p1 <**> p2 <**> p3 <**> p4
-}

infixl 4 <$$>, <$$, <**>, <**, **>

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAlt'` pEmpty []

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = (:) <$$> p <**> pMunch p

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = (:) <$$> p <**> pMunch (sep **> p)

takeFirstParse :: [(a, [Token])] -> a
takeFirstParse = \ case
  (x, []) : _ 
    -> x
  (_, (i,_) : _): ps 
    -> case ps of
      _ : _ -> takeFirstParse ps
      []    -> error $ "syntax error at line " ++ show i
  _ -> error  "syntax error at line 1"
