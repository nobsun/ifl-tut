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

lex :: String -> [Token]
lex inp = concatMap (ana psi) (zip [1 :: Int ..] (lines inp)) where
    psi = \ case
        (lc,cs) -> case dropWhile isSpace cs of
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
            c:_ | isSymbol c -> case span isSymbol cs of
                (bop,rs)         -> Cons (lc,LBop bop) (lc,rs)
                | isDigit c  -> case span isDigit cs of
                    (ds,rs)      -> Cons (lc,LNum (read ds)) (lc,rs)
                | isAlpha c  -> case span isIden cs of
                    (vs,rs)
                        | S.member vs reserveds
                                 -> Cons (lc,LRsv vs) (lc,rs)
                        | otherwise
                                 -> Cons (lc,LIde vs) (lc,rs)
                | otherwise  -> Cons (lc, LUnkn c) (lc, drop 1 cs)

isBopSymbol :: Char -> Bool
isBopSymbol = flip S.member bopSymbols

bopSymbols :: S.Set Char
bopSymbols = S.fromList
    ("!#$%&*+-./:<=>?@^|~" :: String)

isIden :: Char -> Bool
isIden c = isAlpha c || isDigit c || c == '_'