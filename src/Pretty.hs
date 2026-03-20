-- # Pretty
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Pretty
    where

import GHC.Generics (Generic)
import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Language
import Iseq
import Bop

tabSize :: Int
tabSize = 4

iTab :: IseqRep
iTab = iSpaces tabSize

{- pretty printer -}
pprint :: CoreProgram -> String
pprint = pprintGen iStr

pprintGen :: VarRep a
          => (a -> IseqRep)
          -> Program a
          -> String
pprintGen ppr
    = iDisplay . pprProgramGen ppr

pprProgramGen :: VarRep a
              => (a -> IseqRep)
              -> Program a
              -> IseqRep
pprProgramGen ppr
    = iInterleave (iAppend (iStr ";") iNewline) . map (pprScDefnGen ppr)

pprScDefnGen :: VarRep a
             => (a -> IseqRep)
             -> ScDefn a
             -> IseqRep
pprScDefnGen ppr = \ case
    (name,args,body) 
        -> iConcat [ iStr name
                   , pprArgsGen ppr args
                   , iSpace, iStr "=", iSpace
                   , iIndent (pprExprGen ppr body)
                   ]

pprArgsGen :: (a -> IseqRep)
           -> [a]
           -> IseqRep
pprArgsGen ppr args
    = if null args then iNil
      else iConcat [iSpace, iInterleave iSpace (map ppr args)]

{- |
>>> es1 = [varSampleE, numSampleE]
>>> es2 = [constrSampleE10, constrSampleE21]
>>> es3 = [appInfixSampleE, appInfixSampleE1, appInfixSampleE2, appInfixSampleE3, appInfixSampleE4]
>>> es4 = [letSampleE, caseSampleE, lambdaSampleE]
>>> es = concat [es1,es2,es3,es4]
>>> putStrLn $ iDisplay $ iLayn2 $ map pprExpr es
     1) var
     2) 57
     3) Pack{1,0}
     4) Pack{2,1}
     5) x + y < p * length xs
     6) 12 / 2 / (6 / 3)
     7) 12 * 2 / (6 * 3)
     8) 12 * 2 * (6 * 3)
     9) 12 * 2 - 6 * 3
    10) letrec
            y = x + 1;
            z = Y * 2
        in
            z
    11) case xxs of
            <1> → 0;
            <2> x xs → 1 + length xs
    12) λ x y → Pack{1,2} x y
<BLANKLINE>
-}
pprExpr :: CoreExpr
        -> IseqRep
pprExpr = pprExprGen iStr

pprExprGen :: forall a. VarRep a
           => (a -> IseqRep)
           -> Expr a
           -> IseqRep
pprExprGen ppr = snd . histoExpr phi where
    phi :: ExprF a (AnnExpr a (Precedence, IseqRep)) -> (Precedence, IseqRep)
    phi = \ case
        EVarF v -> (maxBound, v') where
            v' = bool iStr (pprBop True) (isBop v) v
        ENumF n -> (maxBound, iNum n)
        EConstrF t a -> (maxBound, doc) where
            doc = iConcat [ iStr "Pack{", iNum t, iStr ",", iNum a, iStr "}" ]
        EApF e1 e2 -> case e1 of
            _ :< EApF e11 e12 -> case e11 of
                _ :< EVarF bop
                    | isBop bop -> case bopInfoOf bop of
                        (p,Infix)  -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _ -> (p, doc) where
                                                 doc = iConcat 
                                                     [ iParen' (p >= o1) do1
                                                     , iSpace, pprBop False bop, iSpace
                                                     , iParen' (p >= o2) do2
                                                     ]
                        (p,InfixL) -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _ -> (p, doc) where
                                                 doc = iConcat 
                                                     [ iParen' (p >  o1) do1
                                                     , iSpace, pprBop False bop, iSpace
                                                     , iParen' (p >= o2) do2
                                                     ]
                        (p,InfixR) -> case e12 of
                            (o1,do1) :< _ -> case e2 of
                                (o2,do2) :< _ -> (p, doc) where
                                                 doc = iConcat 
                                                     [ iParen' (p >= o1) do1
                                                     , iSpace, pprBop False bop, iSpace
                                                     , iParen' (p >  o2) do2
                                                     ]
                    | otherwise -> pprEAp e1 e2
                _ -> pprEAp e1 e2
            _ :< EVarF bop
                | isBop bop -> case bopInfoOf bop of
                    (p,Infix)  -> case e2 of
                        (q, do1) :< _ -> (maxBound, iParen doc) where
                            doc = iConcat
                                [ iParen' (p >= q) do1
                                , iSpace, pprBop False bop
                                ]
                    (p,InfixL) -> case e2 of
                        (q, do1) :< _ -> (maxBound, iParen doc) where
                            doc = iConcat
                                [ iParen' (p > q) do1
                                , iSpace, pprBop False bop
                                ]
                    (p,InfixR) -> case e2 of
                        (q, do1) :< _ -> (maxBound, iParen doc) where
                            doc = iConcat
                                [ iParen' (p >= q) do1
                                , iSpace, pprBop False bop
                                ]
            _ -> pprEAp e1 e2
        ELetF isRec defns body -> (minBound, doc) where
            doc = iConcat
                [ iStr "let", bool iNil (iStr "rec") isRec, iNewline
                , iTab, iIndent defns', iNewline
                , iStr "in", iNewline
                , iTab, iIndent body'
                ]
            defns' = pprBinders defns
            body'  = case body of (_,e) :< _ -> e

        ECaseF e alts -> (minBound, doc) where
            doc = iConcat
                [ iStr "case", iSpace, e', iSpace, iStr "of", iNewline
                , iTab, iIndent alts'
                ]
            e' = case e of
                (_,de) :< _ -> de
            alts' = pprAlts alts
                
        ELamF args body -> case sectionType args body of
            NotSection      -> pprELam args body
            SectionBoth bop -> (maxBound, bop') where
                bop' = pprBop True bop
            SectionL bop info (p,d1) -> case info of                 -- (x +)
                (o,Infix)  -> (maxBound, iParen sec) where
                    sec = iConcat 
                        [ iParen' (o >= p) d1, iSpace, iStr bop ]
                (o,InfixL) -> (maxBound, iParen secl) where
                    secl = iConcat 
                         [ iParen' (o >= p) d1, iSpace, iStr bop ]
                (o,InfixR) -> (maxBound, iParen secl) where
                    secl = iConcat
                         [ iParen' (o >  p) d1, iSpace, iStr bop ]
            SectionR bop info (q,d2) -> case info of                 -- (+ y)
                (o,Infix)  -> (maxBound, iParen secr) where
                    secr = iConcat
                         [ iStr bop, iSpace, iParen' (o >= q) d2 ]
                (o,InfixL) -> (maxBound, iParen secr) where
                    secr = iConcat
                         [ iStr bop, iSpace, iParen' (o >  q) d2 ]
                (o,InfixR) -> (maxBound, iParen secr) where
                    secr = iConcat
                         [ iStr bop, iSpace, iParen' (o >= q) d2 ]

    pprEAp :: AnnExpr a (Precedence, IseqRep)
           -> AnnExpr a (Precedence, IseqRep)
           -> (Precedence, IseqRep)
    pprEAp fun arg = case (pprFun fun, pprArg arg) of
        (dfun, darg) -> (10 :: Int, doc) where
            doc = iConcat
                [ dfun, iSpace, darg]
        where
            pprFun = \ case
                (p,doc) :< _ -> iParen' (p < 10) doc
            pprArg = \ case
                (p,doc) :< _ -> iParen' (p < maxBound) doc

    pprBinders :: AnnBinders a (Precedence, IseqRep)
               -> IseqRep
    pprBinders = iInterleave sep . map pprBinder where
        pprBinder = \ case
            (a,(_,rhs) :< _) -> iConcat [ppr a, iStr " = ", rhs]
        sep = iConcat [iSemi, iNewline]

    pprAlts :: AnnAlters a (Precedence, IseqRep)
            -> IseqRep
    pprAlts = iInterleave sep . map pprAlt where
        pprAlt = \ case
            (t,as,(_,rhs) :< _) -> iConcat
                [ iAngle (iNum t)
                , pprArgs ppr as, iSpace
                , iStr "→", iSpace
                , rhs
                ]
        sep = iConcat [iSemi, iNewline]

    pprELam :: [a] 
            -> AnnExpr a (Precedence, IseqRep)
            -> (Precedence, IseqRep)
    pprELam xs body = (minBound, lam) where
        lam = case body of
            (_,body') :< _ -> iConcat
                [ iStr "λ"
                , pprArgs ppr xs, iSpace
                , iStr "→", iSpace
                , body'
                ]

pprBop :: Bool -> Name -> IseqRep
pprBop solo = \ case
    bop | "`" `isPrefixOf` bop -> iStr (bool id trim solo bop)
        | otherwise            -> bool id iParen solo (iStr bop)
    where
        trim = dropWhileEnd ('`' ==) . dropWhile ('`' ==)

pprArgs :: (a -> IseqRep) -> [a] -> IseqRep
pprArgs ppr = \ case
    [] -> iNil
    xs -> iConcat [iSpace, iInterleave iSpace (map ppr xs)]

data SectionType d
    = NotSection
    | SectionBoth Name
    | SectionL Name BopInfo d
    | SectionR Name BopInfo d

sectionType :: VarRep a 
            => [a] 
            -> AnnExpr a (Int, IseqRep)
            -> SectionType (Int, IseqRep)
sectionType xs body = case xs of
    [x] -> case body of
        _ :< EApF e1 e2 -> case e1 of
            _ :< EApF e11 e12 -> case e11 of
                _ :< EVarF bop
                    | isBop bop -> case e12 of
                        ae12 :< EVarF y -> case e2 of
                            ae2 :< EVarF z
                                | y == z       -> NotSection
                                | vname x == y -> SectionR bop (bopInfoOf bop) ae2
                                | vname x == z -> SectionL bop (bopInfoOf bop) ae12
                                | otherwise    -> NotSection
                            ae2 :< _
                                | vname x == y -> SectionR bop (bopInfoOf bop) ae2
                                | otherwise    -> NotSection
                        ae12 :< _       -> case e2 of
                            _ :< EVarF z
                                | vname x == z -> SectionL bop (bopInfoOf bop) ae12
                                | otherwise    -> NotSection
                            _                  -> NotSection
                _               -> NotSection
            _                 -> NotSection
        _               -> NotSection
    [x,y] -> case body of
        _ :< EApF e1 e2 -> case e2 of
            _ :< EVarF w
                | vname y /= w  -> NotSection
                | otherwise     -> case e1 of
                    _ :< EApF e11 e12 -> case e11 of
                        _ :< EVarF bop
                            | isBop bop   -> case e12 of
                                _ :< EVarF z
                                    | vname x == z -> SectionBoth bop
                                _                  -> NotSection
                        _                 -> NotSection
                    _                 -> NotSection
            _               -> NotSection
        _               -> NotSection
    _   -> NotSection

type Binders a = [Binder a]

pprDefnsGen :: VarRep a
            => (a -> IseqRep)
            -> Binders a
            -> IseqRep
pprDefnsGen ppr
    = iInterleave sep . map (pprDefnGen ppr)
    where
        sep = iConcat [ iStr ";", iNewline ]

pprDefnGen :: VarRep a
           => (a -> IseqRep)
           -> (a, Expr a)
           -> IseqRep
pprDefnGen ppr (name, expr)
    = iConcat [ ppr name, iStr " = ", iIndent (pprExprGen ppr expr) ]

{- AnnProgram -}

-- type AnnProgram a ann = [AnnScDefn a ann]
-- type AnnScDefn a ann = (Name, [a], AnnExpr a ann)
-- type AnnExpr a = Cofree (ExprF a)
-- type AnnBinders a ann = [(a, AnnExpr a ann)]

{- pprintAnn -}
pprintAnn :: VarRep a 
          => (a   -> IseqRep)
          -> (ann -> IseqRep)
          -> AnnProgram a ann
          -> String
pprintAnn ppr annppr
    = iDisplay . pprAnnProgram ppr annppr

pprAnnProgram :: VarRep a
              => (a   -> IseqRep)
              -> (ann -> IseqRep)
              -> AnnProgram a ann
              -> IseqRep
pprAnnProgram ppr annppr
    = iInterleave (iAppend (iStr ";") iNewline) . map (pprAnnScDefn ppr annppr)

pprAnnScDefn :: VarRep a
             => (a   -> IseqRep)
             -> (ann -> IseqRep)
             -> AnnScDefn a ann
             -> IseqRep
pprAnnScDefn ppr annppr = \ case
    (name,args,body)
        -> iConcat [ iStr name
                   , pprArgs ppr args
                   , iSpace, iStr "=", iSpace
                   , iIndent (pprAnnExpr ppr annppr body)
                   ]

pprAnnExpr :: forall a ann. VarRep a
           => (a -> IseqRep)
           -> (ann -> IseqRep)
           -> AnnExpr a ann
           -> IseqRep
pprAnnExpr ppr annppr = snd . histoAnnExpr phi where
    phi :: F.CofreeF (ExprF a) ann (Cofree (F.CofreeF (ExprF a) ann) (Precedence, IseqRep)) -> (Precedence, IseqRep)
    phi = \ case
        ann F.:< EVarF v      -> (maxBound, v') where
            v' = iAnn ann (bool iStr (pprBop True) (isBop v) v)
        ann F.:< ENumF n      -> (maxBound, n') where n' = iAnn ann (iNum n)
        ann F.:< EConstrF t a -> (maxBound, c')  where 
            c'  = iAnn ann c
            c   = iConcat [iStr "Pack{", iNum t, iStr ",", iNum a, iStr "}"]
        ann F.:< EApF e1 e2 -> case e1 of
            _ :< ann1 F.:< EApF e11 e12 -> case e11 of
                _ :< ann11 F.:< EVarF bop
                    | isBop bop -> case bopInfoOf bop of
                        (p,Infix)   -> case e12 of
                            (o1,do1) :< _ann12 F.:< _ -> case e2 of
                                (o2,do2) :< _ann2 F.:< _ -> (p, doc) where
                                    doc = iAnn ann $ iConcat
                                        [ iAnn ann1 $ iConcat [ iParen' (p >= o1) do1
                                                              , iSpace, iAnn ann11 (pprBop False bop)
                                                              ]
                                        , iSpace, iParen' (p >= o2) do2
                                        ]
                        (p,InfixL)  -> case e12 of
                            (o1,do1) :< _ann12 F.:< _ -> case e2 of
                                (o2,do2) :< _ann2 F.:< _ -> (p, doc) where
                                    doc = iAnn ann $ iConcat
                                        [ iAnn ann1 $ iConcat [ iParen' (p >  o1) do1
                                                              , iSpace, iAnn ann11 (pprBop False bop)
                                                              ]
                                        , iSpace , iParen' (p >= o2) do2
                                        ]
                        (p,InfixR)  -> case e12 of
                            (o1,do1) :< _ann12 F.:< _ -> case e2 of
                                (o2,do2) :< _ann2 F.:< _ -> (p, doc) where
                                    doc = iAnn ann $ iConcat
                                        [ iAnn ann1 $ iConcat [ iParen' (p >= o1) do1
                                                              , iSpace, iAnn ann11 (pprBop False bop)
                                                              ]
                                        , iSpace, iParen' (p >  o2) do2
                                        ]
                    | otherwise -> second (iAnn ann) $ pprAnnEAp e1 e2
                _ -> second (iAnn ann) $ pprAnnEAp e1 e2
            _ :< ann1 F.:< EVarF bop
                | isBop bop -> case bopInfoOf bop of
                    (p,Infix)  -> case e2 of
                        (q,do1) :< ann2 F.:< _ -> (maxBound, iParen doc) where
                            doc = iAnn ann $ iConcat
                                [ iAnn ann2 $ iParen' (p >= q) do1
                                , iSpace, iAnn ann1 (pprBop False bop)
                                ]
                    (p,InfixL) -> case e2 of
                        (q,do1) :< ann2 F.:< _ -> (maxBound, iParen doc) where
                            doc = iAnn ann $ iConcat
                                [ iAnn ann2 $ iParen' (p > q) do1
                                , iSpace, iAnn ann1 (pprBop False bop)
                                ]
                    (p,InfixR) -> case e2 of
                        (q,do1) :< ann2 F.:< _ -> (maxBound, iParen doc) where
                            doc = iAnn ann $ iConcat
                                [ iAnn ann2 $ iParen' (p >= q) do1
                                , iSpace, iAnn ann1 (pprBop False bop)
                                ]
            _ -> second (iAnn ann) $ pprAnnEAp e1 e2
        ann F.:< ELetF isRec defns body -> (minBound, dlet) where
            dlet = iAnn ann $ iConcat
                [ iStr "let", bool iNil (iStr "rec") isRec, iNewline
                , iTab, iIndent defns', iNewline
                , iStr "in", iNewline
                , iTab, iIndent body'
                ]
            defns' = pprAnnBinders defns
            body'  = case body of (_,e) :< _ F.:< _ -> e
        ann F.:< ECaseF ((_,e) :< _) alts -> (minBound, dcase) where
            dcase = iAnn ann $ iConcat
                [ iStr "case", iSpace, e, iSpace, iStr "of", iNewline
                , iTab, iIndent alts'
                ]
            alts' = pprAnnAlts alts
        ann F.:< ELamF xs body -> (minBound, lam) where
            lam = case body of
                (_,body') :< _ -> iAnn ann $ iConcat
                    [ iStr "λ", pprArgs ppr xs, iSpace
                    , iStr "→", iSpace
                    , body'
                    ]

    iAnn :: ann -> IseqRep -> IseqRep
    iAnn ann iseq = iDAngle (iConcat [annppr ann, iStr ",", iseq])

    pprAnnEAp :: Cofree (F.CofreeF (ExprF a) ann) (Precedence, IseqRep)
              -> Cofree (F.CofreeF (ExprF a) ann) (Precedence, IseqRep)
              -> (Precedence, IseqRep)
    pprAnnEAp fun arg = case (pprAnnFun fun, pprAnnArg arg) of
        (dfun, darg) -> (10 :: Int, fapp) where
            fapp = iConcat [dfun, iSpace, darg]
        where
            pprAnnFun = \ case
                (p,doc) :< _annf F.:< _ -> iParen' (p < 10) doc
            pprAnnArg = \ case
                (p,doc) :< _annarg F.:< _ -> iParen' (p < 10) doc

    pprAnnBinders :: BindersF a (Cofree (F.CofreeF (ExprF a) ann) (Precedence, IseqRep)) -> IseqRep
    pprAnnBinders = iInterleave sep . map pprAnnBinder where
        pprAnnBinder = \ case
            (x, (_,rhs) :< _) -> iConcat [ppr x, iSpace, iStr "=", iSpace, rhs]
        sep = iConcat [iSemi, iNewline]

    pprAnnAlts :: [(Tag, [a], Cofree (F.CofreeF (ExprF a) ann) (Precedence, IseqRep))] -> IseqRep
    pprAnnAlts = iInterleave sep . map pprAnnAlt where
        pprAnnAlt = \ case
            (t, xs, (_,rhs) :< _) -> iConcat
                [ iAngle (iNum t)
                , pprArgs ppr xs
                , iSpace, iStr "→", iSpace
                , rhs
                ]
        sep = iConcat [iSemi, iNewline]

pprAnnDefns :: VarRep a
            => (a   -> IseqRep)
            -> (ann -> IseqRep)
            -> AnnBinders a ann
            -> IseqRep
pprAnnDefns ppr annppr 
    = iInterleave sep . map (pprAnnDefn ppr annppr)
    where
        sep = iConcat [ iStr ";", iNewline ]

pprAnnDefn :: VarRep a
           => (a -> IseqRep) 
           -> (ann -> IseqRep)
           -> (a, AnnExpr a ann)
           -> IseqRep
pprAnnDefn ppr annppr (name, annexpr)
    = iConcat [ ppr name
              , iSpace, iStr "=", iSpace
              , iIndent (pprAnnExpr ppr annppr annexpr) 
              ]

pprintAnnExpr :: (ann -> IseqRep) -> AnnExpr Name ann -> String
pprintAnnExpr pprann = iDisplay . pprAnnExpr iStr pprann
