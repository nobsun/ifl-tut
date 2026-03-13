-- # Bop
-- 2項演算子
-- 
-- ## 言語拡張と`module`宣言
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Bop
    where

import Data.Maybe
import Data.Map qualified as M
import Data.Set qualified as S

ascSymbols :: S.Set Char
ascSymbols = S.fromList $ "!#$%&*+./:<=>?@\\^|" ++ "-~:"

isAscSymbol :: Char -> Bool
isAscSymbol = flip S.member ascSymbols

isBopString :: String -> Bool
isBopString = all isAscSymbol 

type Precedence = Int

data Fixity
    = Infix
    | InfixL
    | InfixR
    deriving (Eq, Show, Read)

type BopInfo = (Precedence, Fixity)

preludeOperators :: M.Map String BopInfo
preludeOperators = M.fromList 
    [ ("!!", (9, InfixL))
    , ("." , (9, InfixR))
    , ("^" , (8, InfixR))
    , ("^^", (8, InfixR))
    , ("**", (8, InfixR))
    , ("*",  (7, InfixL))
    , ("/",  (7, InfixL))
    , ("`div`", (7, InfixL))
    , ("`mod`", (7, InfixL))
    , ("`quot`", (7, InfixL))
    , ("`rem`", (7, InfixL))
    , ("+",  (6, InfixL))
    , ("-",  (6, InfixL))
    , (":",  (5, InfixR))
    , ("++", (5, InfixR))
    , ("==", (4, Infix))
    , ("/=", (4, Infix))
    , ("<",  (4, Infix))
    , ("<=", (4, Infix))
    , (">",  (4, Infix))
    , (">=", (4, Infix))
    , ("`elem`", (4, Infix))
    , ("`notElem`", (4, Infix))
    , ("&&", (3, InfixR))
    , ("||", (2, InfixR))
    , (">>", (1, InfixL))
    , (">>=", (1, InfixL))
    , ("$",  (0, InfixR))
    , ("$!", (0, InfixR))
    , ("`seq`", (0, InfixL))
    ]

operators :: M.Map String BopInfo
operators = M.unions [preludeOperators]

isBop :: String -> Bool
isBop op = M.member op 
         $ operators

bopInfoOf :: String -> BopInfo
bopInfoOf op
    = fromMaybe (error $ "bopInfoOf: unknown operator" ++ op)
    $ operators M.!? op

precedenceOf :: String -> Int
precedenceOf op 
    = maybe (error $ "precedenceOf: unknown operator " ++ show op) fst 
    $ operators M.!? op

fixityOf :: String -> Fixity
fixityOf op
    = maybe (error $ "fixityOf: unknown operator" ++ show op) snd 
    $ operators M.!? op

{- ^
>>> isBop "++"
True
>>> isBop "+++"
False
>>> precedenceOf "&&"
3
>>> precedenceOf "&&&"
precedenceOf: unknown operator "&&&"
>>> fixityOf ">>"
InfixL
>>> fixityOf ">>>"i
fixityOf: unknown operator">>>"
-}
