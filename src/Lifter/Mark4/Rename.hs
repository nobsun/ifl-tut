-- # Lifter.Mark4.Rename
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Rename
    where

import Data.Char
import Data.Function
import Data.List
import Data.Set qualified as S
-- import Text.ParserCombinators.ReadP
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Lambda
import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.Mark3.Collect

type NameSupply = Int

initialNameSupply :: NameSupply
initialNameSupply = 0

rename :: CoreProgram -> CoreProgram
rename = snd . mapAccumL renameSc initialNameSupply
    where
        renameSc ns (scName, args, rhs)
            = (ns2, (scName, args', rhs'))
            where
                (ns1, args', env) = newNames ns args
                (ns2, rhs')       = renameExpr env ns1 rhs

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], Assoc Name Name)
newNames ns olds
    = (ns', news, env)
    where
        (ns',news) = getNames ns olds
        env = zip olds news

getNames :: NameSupply -> [Name] -> (NameSupply, [Name])
getNames ns prefixes
    = (ns + length prefixes, zipWith makeName prefixes [ns ..])

getName :: NameSupply -> Name -> (NameSupply, Name)
getName ns prefix = (succ ns, makeName prefix ns)

makeName :: Name -> NameSupply -> Name
makeName prefix ns = prefix ++ "_" ++ show ns

renameExpr :: Assoc Name Name
           -> NameSupply
           -> CoreExpr
           -> (NameSupply, CoreExpr)
renameExpr env ns expr = case expr of

    EVar v -> (ns, EVar (aLookup env v v))

    ENum _ -> (ns, expr)

    EAp e1 e2 -> (ns2, EAp e1' e2')
        where
            (ns1, e1') = renameExpr env ns e1
            (ns2, e2') = renameExpr env ns1 e2

    ELam args body
        -> (ns2, ELam args' body')
        where
            (ns1, args', env') = newNames ns args
            (ns2, body')       = renameExpr (env' ++ env) ns1 body

    ELet isRec defns body
        -> (ns3, ELet isRec (zip binders' rhss') body')
        where
            (ns1, body') = renameExpr bodyEnv ns body
            binders = bindersOf defns
            (ns2, binders', env') = newNames ns1 binders
            bodyEnv = env' ++ env
            (ns3, rhss') = mapAccumL (renameExpr rhsEnv) ns2 (rhssOf defns)
            rhsEnv | isRec     = bodyEnv
                   | otherwise = env
    
    EConstr _ _ -> (ns, expr)

    ECase e alts -> case renameExpr env ns e of
        (ns1, e')    -> case mapAccumL (renameAlt env) ns1 alts of
            (ns2, alts') -> (ns2, ECase e' alts')

renameAlt :: Assoc Name Name -> NameSupply -> CoreAlt -> (NameSupply, CoreAlt)
renameAlt env ns (t,as,e)
    = case newNames ns as of
        (ns1,as',env') -> case renameExpr (env' ++ env) ns1 e of
            (ns2,e')       -> (ns2, (t,as',e'))
