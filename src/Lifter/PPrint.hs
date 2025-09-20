-- # Lifter.PPrint
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Lifter.PPrint
    where

import GHC.Generics (Generic)
import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable
import Data.Bool
import Data.Char
import Data.Maybe
import Language hiding (pprint, pprExpr)
import Iseq

data ExprF a r
    = EVarF Name
    | ENumF Int
    | EConstrF
        Tag
        Arity
    | EApF r r
    | ELetF
        IsRec
        (BindersF a r)
        r
    | ECaseF
        r 
        (AltersF a r)
    | ELamF
        [a]
        r
    deriving (Eq, Show, Functor, Generic)

type BindersF a r = [(a, r)]

type AltersF a r = [(Tag, [a], r)]

type instance Base (Expr a) = ExprF a

instance Recursive (Expr a) where
    project = \ case
        EVar n -> EVarF n
        ENum n -> ENumF n
        EConstr tag ary -> EConstrF tag ary
        EAp e1 e2       -> EApF e1 e2
        ELet isrec bs e -> ELetF isrec bs e
        ECase e alts    -> ECaseF e alts
        ELam xs e       -> ELamF xs e

instance Corecursive (Expr a) where
    embed = \ case
        EVarF n -> EVar n
        ENumF n -> ENum n
        EConstrF tag ary -> EConstr tag ary
        EApF e1 e2       -> EAp e1 e2
        ELetF isrec bs e -> ELet isrec bs e
        ECaseF e alts    -> ECase e alts
        ELamF xs e       -> ELam xs e

{- pretty printer -}
pprint :: CoreProgram -> String
pprint = pprintGen iStr

pprintGen :: (a -> IseqRep)
          -> Program a
          -> String
pprintGen ppr
    = iDisplay . pprProgramGen ppr

pprProgramGen :: (a -> IseqRep)
              -> Program a
              -> IseqRep
pprProgramGen ppr
    = iInterleave (iAppend (iStr " ;") iNewline) . map (pprScDefnGen ppr)

pprScDefnGen :: (a -> IseqRep)
             -> ScDefn a
             -> IseqRep
pprScDefnGen ppr = \ case
    (name,args,body) 
        -> iConcat [ iStr name
                   , if null args 
                        then iNil 
                        else iAppend iSpace (pprArgsGen ppr args)
                   , iStr " = "
                   , iIndent (pprExprGen ppr 0 body)
                   ]

pprArgsGen :: (a -> IseqRep)
           -> [a]
           -> IseqRep
pprArgsGen ppr
    = iConcat . map (iAppend iSpace . ppr)

{- |
>>> es1 = [varSampleE, numSampleE]
>>> es2 = [constrSampleE10, constrSampleE21]
>>> es3 = [appInfixSampleE, appInfixSampleE1, appInfixSampleE2, appInfixSampleE3, appInfixSampleE4]
>>> es4 = [letSampleE, caseSampleE, lambdaSampleE]
>>> es = concat [es1,es2,es3,es4]
>>> putStrLn $ iDisplay $ iLayn $ map (pprExpr 0) es
     1) var
     2) 57
     3) Pack{1,0}
     4) Pack{2,1}
     5) x + y < p * length xs
     6) (12 / 2) / (6 / 3)
     7) (12 * 2) / (6 * 3)
     8) (12 * 2) * (6 * 3)
     9) 12 * 2 - 6 * 3
    10) letrec
          y = x + 1;
          z = Y * 2
        in z
    11) case xxs of
          <1> -> 0;
          <2> x xs -> 1 + length xs
    12) \ x y -> Pack{1,2} x y
<BLANKLINE>
-}
pprExpr :: Precedence
        -> CoreExpr
        -> IseqRep
pprExpr = pprExprGen iStr

pprExprGen :: (a -> IseqRep)
           -> Precedence
           -> Expr a
           -> IseqRep
pprExprGen ppr p = para phi where
    phi = \ case
        EVarF v -> iStr v
        ENumF n -> iNum n
        EConstrF tag ary
            -> iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum ary, iStr "}" ]
        EApF (EAp (EVar op) e1, _) (e2, _)
            | isJust mfx -> bool id iParen (p >= p') infixexpr
            where
                mfx = lookup op binOps
                (p',_) = fromJust mfx
                infixexpr = iConcat
                          [ pprExprGen ppr p' e1
                          , iSpace, iStr op, iSpace
                          , pprExprGen ppr p' e2
                          ]
        EApF (e1,_) (e2,_) -> bool id iParen (p > 6) appexpr
            where
                appexpr = iConcat [ pprExprGen ppr 6 e1
                                  , iStr " "
                                  , pprExprGen ppr 7 e2 ]
        ELetF isrec defns body 
            -> bool id iParen (p > 0) letexpr
            where
                letexpr = iConcat [ iStr keyword, iNewline
                                  , iStr "  ", iIndent (pprDefnsGen ppr (map (second fst) defns)), iNewline
                                  , iStr "in ", pprExprGen ppr 0 (fst body) ]
                keyword | isrec     = "letrec"
                        | otherwise = "let"
        ECaseF e alts
            -> bool id iParen (p > 0) caseexpr
            where
                third f (x,y,z) = (x,y,f z)
                caseexpr = iConcat [ iStr "case ", iIndent (pprExprGen ppr 0 (fst e)), iStr " of", iNewline
                                   , iStr "  ", iIndent (iInterleave iNl (map (pprAlt . third fst) alts)) ]
                iNl = iConcat [ iStr ";", iNewline ]
                pprAlt (tag, args, rhs)
                    = iConcat [ iStr "<", iNum tag, iStr ">"
                              , pprArgsGen ppr args, iStr " -> "
                              , iIndent (pprExprGen ppr 0 rhs) ]
        ELamF args body
            -> bool id iParen (p > 0) lambda
            where
                lambda = iConcat [ iStr "\\", pprArgsGen ppr args
                                 , iStr " -> "
                                 , iIndent (pprExprGen ppr 0 (fst body)) ]

type Binders a = [(a, Expr a)]

pprDefnsGen :: (a -> IseqRep)
            -> Binders a
            -> IseqRep
pprDefnsGen ppr
    = iInterleave sep . map (pprDefnGen ppr)
    where
        sep = iConcat [ iStr ";", iNewline ]

pprDefnGen :: (a -> IseqRep)
           -> (a, Expr a)
           -> IseqRep
pprDefnGen ppr (name, expr)
    = iConcat [ ppr name, iStr " = ", iIndent (pprExprGen ppr 0 expr) ]

{- AnnProgram -}

type AnnProgram a ann = [AnnScDefn a ann]
type AnnScDefn a ann = (Name, [a], AnnExpr a ann)
type AnnExpr a = Cofree (ExprF a)
type AnnBinders a ann = [(a, AnnExpr a ann)]

{- pprintAnn -}
pprintAnn :: (a   -> IseqRep)
          -> (ann -> IseqRep)
          -> AnnProgram a ann
          -> String
pprintAnn ppr annppr
    = iDisplay . pprAnnProgram ppr annppr

pprAnnProgram :: (a   -> IseqRep)
              -> (ann -> IseqRep)
              -> AnnProgram a ann
              -> IseqRep
pprAnnProgram ppr annppr
    = iInterleave (iAppend (iStr ";") iNewline) . map (pprAnnScDefn ppr annppr)

pprAnnScDefn :: (a   -> IseqRep)
             -> (ann -> IseqRep)
             -> AnnScDefn a ann
             -> IseqRep
pprAnnScDefn ppr annppr = \ case
    (name,args,body)
        -> iConcat [ iStr name
                   , if null args
                        then iNil
                        else iAppend iSpace (pprArgsGen ppr args)
                   , iStr "="
                   , iIndent (pprAnnExpr ppr annppr 0 body)
                   ]

pprAnnExpr :: forall a ann. (a   -> IseqRep)
           -> (ann -> IseqRep)
           -> Precedence
           -> AnnExpr a ann
           -> IseqRep
pprAnnExpr ppr annppr p = para phi where
    phi :: F.CofreeF (ExprF a) ann (AnnExpr a ann, IseqRep) -> IseqRep
    phi (ann F.:< exprf) = case exprf of
        EVarF v -> iAnn ann (iStr v)
        ENumF n -> iAnn ann (iNum n)
        EConstrF tag ary 
            -> iAnn ann iseq where
                iseq = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum ary, iStr "}" ]
        EApF (_ :< EApF (_ :< EVarF op) e1, _) (e2, _)
            | isJust mfx -> iAnn ann infixexpr
            where
                mfx = lookup op binOps
                (p',_) = fromJust mfx
                infixexpr = bool id iParen (p >= p')
                          $ iConcat
                          [ pprAnnExpr ppr annppr p' e1
                          , iSpace, iStr op, iSpace
                          , pprAnnExpr ppr annppr p' e2
                          ]
        EApF (e1,_) (e2,_) -> iAnn ann appexpr
            where
                appexpr = bool id iParen (p > 6)
                        $ iConcat [ pprAnnExpr ppr annppr 6 e1
                                  , iStr " "
                                  , pprAnnExpr ppr annppr 7 e2 ]
        ELetF isrec defns body
            -> iAnn ann letexpr
            where
                letexpr = bool id iParen (p > 0)
                        $ iConcat
                        [ iStr keyword, iNewline
                        , iStr "  "
                        , iIndent (pprAnnDefns ppr annppr (map (undefined) defns)), iNewline
                        , iStr "in ", pprAnnExpr ppr annppr 0 (fst body) ]
                keyword | isrec     = "letrec"
                        | otherwise = "let"
        ECaseF e alts
            -> iAnn ann caseexpr
            where
                third f (x,y,z) = (x,y,f z)
                caseexpr = bool id iParen (p > 0)
                         $ iConcat
                         [ iStr "case ", iIndent (pprAnnExpr ppr annppr 0 (fst e)), iStr " of", iNewline
                         , iStr "  ", iIndent (iInterleave iNl (map (pprAnnAlt . third fst) alts))
                         ]
                iNl = iConcat [ iStr ";", iNewline ]
                pprAnnAlt (tag, args, rhs)
                    = iConcat [ iStr "<", iNum tag, iStr ">"
                              , pprArgsGen ppr args, iStr " -> "
                              , iIndent (pprAnnExpr ppr annppr 0 rhs)]
        ELamF args body
            -> iAnn ann lambda
            where
                lambda = bool id iParen (p > 0)
                       $ iConcat
                       [ iStr "\\", pprArgsGen ppr args
                       , iStr " -> "
                       , iIndent (pprAnnExpr ppr annppr 0 (fst body))
                       ]

    iAnn :: ann -> IseqRep -> IseqRep
    iAnn ann iseq = iParen (iConcat [annppr ann, iStr ",", iseq])

pprAnnDefns :: (a   -> IseqRep)
            -> (ann -> IseqRep)
            -> AnnBinders a ann
            -> IseqRep
pprAnnDefns ppr annppr 
    = iInterleave sep . map (pprAnnDefn ppr annppr)
    where
        sep = iConcat [ iStr ";", iNewline ]

pprAnnDefn :: (a -> IseqRep) 
           -> (ann -> IseqRep)
           -> (a, AnnExpr a ann)
           -> IseqRep
pprAnnDefn ppr annppr (name, annexpr)
    = iConcat [ ppr name
              , iStr " = "
              , iIndent (pprAnnExpr ppr annppr 0 annexpr) 
              ]
