-- # Lex
{-# LANGUAGE GHC2024 #-}
module Lex
    where

import Data.Char
import Data.Set qualified as S
import Data.Functor.Foldable
import Lexeme

type Loc   = Int
type Token = (Loc, Lexeme)

tokLoc :: Token -> Loc
tokLoc = fst

tokStr :: Token -> String
tokStr = fromLexeme . snd

reserveds :: S.Set String
reserveds = S.fromList
    [ "case"
    , "of"
    , "let"
    , "letrec"
    , "Pack"
    ]

{- |
>>> sample = "main = f 6;\nf x = x + 7"
>>> clex sample
[(1,LIde "main"),(1,LBop "="),(1,LIde "f"),(1,LNum 6),(1,LSemi),(2,LIde "f"),(2,LIde "x"),(2,LBop "="),(2,LIde "x"),(2,LBop "+"),(2,LNum 7)]
-}

clex :: String -> [Token]
clex = concatMap llex . zip [1 ..] . lines

llex :: (Loc, String) -> [Token]
llex = ana psi where
    psi = \ case
        (lc, ccs) -> case ccs' of
            []         -> Nil
            '-':'-':_  -> Nil
            '-':'>':rs -> Cons (lc,LRarr)   (lc,rs)
            '→':rs     -> Cons (lc,LRarr)   (lc,rs)
            '\\':rs    -> Cons (lc,LLambda) (lc,rs)
            'λ':rs     -> Cons (lc,LLambda) (lc,rs)
            '(':rs     -> Cons (lc,LOpar)   (lc,rs)
            ')':rs     -> Cons (lc,LCpar)   (lc,rs)
            '{':rs     -> Cons (lc,LObrc)   (lc,rs)
            '}':rs     -> Cons (lc,LCbrc)   (lc,rs)
            ',':rs     -> Cons (lc,LComm)   (lc,rs)
            ';':rs     -> Cons (lc,LSemi)   (lc,rs)
            c:rs | c == '`'      -> case break ('`' ==) rs of
                ([],_)              -> Cons (lc,LUnkn '`') (lc,rs)
                (_,[])               -> Cons (lc,LUnkn '`') (lc,rs)
                (o,_:rs')             -> Cons (lc,LBop (bq o)) (lc,rs')
                | isBopSymbol c -> case span isBopSymbol ccs' of
                    (bop,rs')        -> Cons (lc,LBop bop) (lc,rs')
                | isDigit c     -> case span isDigit ccs' of
                    (ds,rs')         -> Cons (lc,LNum (read ds)) (lc,rs')
                | isAlpha c     -> case span isIden ccs' of
                    (vs,rs')
                        | S.member vs reserveds
                                -> Cons (lc,LRsv vs) (lc,rs')
                        | otherwise
                                -> Cons (lc,LIde vs) (lc,rs')
                | otherwise  -> Cons (lc, LUnkn c) (lc, drop 1 ccs')
            where
                ccs' = dropWhile isSpace ccs
                bq x = ('`':) . (x ++) . ('`':) $ []

isBopSymbol :: Char -> Bool
isBopSymbol = flip S.member bopSymbols

bopSymbols :: S.Set Char
bopSymbols = S.fromList
    ("!#$%&*+-./:<=>?@^|~" :: String)

isIden :: Char -> Bool
isIden c = isAlpha c || isDigit c || c == '_'
