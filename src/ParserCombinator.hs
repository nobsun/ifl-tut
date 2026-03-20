-- # ParserCombinator
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ParserCombinator
    where

import Control.Arrow hiding ((+++))
import Data.Function
import Data.List
import Lexeme
import Lex

{- 構文解析用の基本的ツール -}

newtype Parser a = Parser { parser :: [Token] -> [(a, [Token])] }

instance Semigroup (Parser a) where
    (<>) :: Parser a -> Parser a -> Parser a
    p <> q = Parser $ uncurry (<>) . (p.parser &&& q.parser)

instance Monoid (Parser a) where
    mempty :: Parser a
    mempty = pFail

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = p { parser = map (first f) . p.parser }

instance Applicative Parser where
    pure :: a -> Parser a
    pure x  = Parser { parser = singleton . (x,) }
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p <*> q = Parser $ \ ts -> [ (f x, ts2) | (f, ts1) <- p.parser ts
                                            , (x, ts2) <- q.parser ts1 ]

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser 
            $ \ ts -> p.parser ts >>= \ (x,ts1) -> 
                      (f x).parser ts1

pSuccess :: a -> Parser a
pSuccess = pure

pFail :: Parser a
pFail = Parser { parser = const [] }

pLit :: Lexeme -> Parser String
pLit lx = fromLexeme <$> pSat (lx ==)

pSat :: (Lexeme -> Bool) -> Parser Lexeme
pSat p = Parser { parser = phi } where
    phi = \ case
        (_,lx) : ts
            | p lx -> [(lx,ts)]
        _          -> []

infixr 5 +++, <++

(+++) :: Parser a -> Parser a -> Parser a
(+++) = (<>)

(<++) :: Parser a -> Parser a -> Parser a
p <++ q = Parser { parser = pq} where
    pq ts = case prs of
        [] -> qrs
        _  -> prs
        where
            prs = p.parser ts
            qrs = q.parser ts

pMany :: Parser a -> Parser [a]
pMany p = pMany1 p +++ pure []

pMany1 :: Parser a -> Parser [a]
pMany1 p = (:) <$> p <*> pMany p

pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p <++ pure []

pMunch1 :: Parser a -> Parser [a]
pMunch1 p = (:) <$> p <*> pMunch p

pManyWithSep :: Parser a -> Parser b -> Parser [a]
pManyWithSep p sep = pMany1WithSep p sep +++ pure []

pMany1WithSep :: Parser a -> Parser b -> Parser [a]
pMany1WithSep p sep = (:) <$> p <*> pMany (sep *> p)

pMunchWithSep :: Parser a -> Parser b -> Parser [a]
pMunchWithSep p sep = pMunch1WithSep p sep <++ pure []

pMunch1WithSep :: Parser a -> Parser b -> Parser [a]
pMunch1WithSep p sep = (:) <$> p <*> pMunch (sep *> p)

pBetween :: Parser a -> Parser a -> Parser b -> Parser b
pBetween lp rp cp = lp *> cp <* rp

pChainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
pChainr p op x = pChainr1 p op +++ pure x

pChainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
pChainr1 p op = scan where
    scan   = p >>= rest
    rest x = (op >>= \ f -> scan >>= \ y -> pure (f x y))
             +++ pure x

pChainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
pChainl p op x = pChainl1 p op +++ pure x

pChainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
pChainl1 p op = p >>= rest where
    rest x = (op >>= \ f -> p >>= \ y -> rest (f x y))
          +++ pure x

pChoice :: [Parser a] -> Parser a
pChoice = foldl1 (+++)

pChoice' :: [Parser a] -> Parser a
pChoice' = foldl1 (<++)

takeFirstParse :: [(a, [Token])] -> a
takeFirstParse res = case res of
    (x,[]):_       -> x
    (_,(i,_):_):ps -> case ps of
        _ : _          -> takeFirstParse ps
        []             -> error $ "takeFirstParse: syntax error at " ++ show i
    _              -> error "takeFirstParse: no parse: " 

takeFirstParse' :: [(a, [Token])] -> [a]
takeFirstParse' res = case res of
    (x,[]):_   -> [x]
    (_,_:_):ps -> case ps of
        _ : _      -> takeFirstParse' ps
        []         -> []
    _          -> []
