-- # Lifter.Mark4.Rename
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.Rename
    ( Level
    , renameL
    , rename
    ) where

import Data.List

import Language
import Utils

type NameSupply = Int

initialNameSupply :: NameSupply
initialNameSupply = 0

type Level = Int

rename :: CoreProgram -> CoreProgram
rename = renameGen newNames

renameL :: Program (Name, a) -> Program (Name, a)
renameL = renameGen newNamesL

newNamesL :: NameSupply 
          -> [(Name, a)]
          -> (NameSupply, [(Name, a)], Assoc Name Name)
newNamesL ns oldbs
    = (ns', newbs, env)
        where
            (olds, levels) = unzip oldbs
            (ns', news) = getNames ns olds
            newbs = zip news levels
            env = zip olds news

renameGen :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name))
                        -- ^ New-binders function
          -> Program a  -- ^ Program to be renamed
          -> Program a  -- ^ Resulting program
renameGen newBinder
    = snd . mapAccumL renameSc initialNameSupply
        where
            renameSc ns (scName, args, rhs)
                = (ns2, (scName, args', rhs'))
                where
                    (ns1, args', env) = newBinder ns args
                    (ns2, rhs') = renameGenExpr newBinder env ns1 rhs

renameGenExpr :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name))
                                       -- ^ New-binders function
              -> Assoc Name Name       -- ^ Maps old names to new ones
              -> NameSupply            -- ^ Name supply
              -> Expr a                -- ^ Expression to be renamed
              -> (NameSupply, Expr a)  -- ^ Depleted name supply and result expression
renameGenExpr newBinder env ns expr = case expr of
    ENum _      -> (ns, expr)
    EConstr _ _ -> (ns, expr)
    EVar v      -> (ns, EVar (aLookup env v v))
    EAp e1 e2   -> (ns2, EAp e1' e2')
        where
            (ns1, e1') = renameGenExpr newBinder env ns  e1
            (ns2, e2') = renameGenExpr newBinder env ns1 e2
    ELam args body
        -> (ns2, ELam args' body')
        where
            (ns1, args', env') = newBinder ns args
            (ns2, body') = renameGenExpr newBinder (env' ++ env) ns1 body
    ELet isRec defns body
        -> (ns3, ELet isRec (zip binders' rhss') body')
        where
            (ns1, body') = renameGenExpr newBinder bodyEnv ns body
            binders = bindersOf defns
            (ns2, binders', env') = newBinder ns1 binders
            bodyEnv = env' ++ env
            (ns3, rhss') = mapAccumL (renameGenExpr newBinder rhsEnv) ns2 (rhssOf defns)
            rhsEnv | isRec     = bodyEnv
                   | otherwise = env
    ECase e alts -> renameGenCase newBinder env ns e alts

renameGenCase :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name))
              -> Assoc Name Name
              -> NameSupply
              -> Expr a
              -> [Alter a]
              -> (NameSupply, Expr a)
renameGenCase newBinder env ns e alts = (ns2, ECase e' alts')
  where
    (ns1, e') = renameGenExpr newBinder env ns e
    (ns2, alts') = mapAccumL (renameGenAlt newBinder env) ns1 alts

renameGenAlt :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name))
             -> Assoc Name Name
             -> NameSupply
             -> Alter a
             -> (NameSupply, Alter a)
renameGenAlt newBinder env ns (tag, args, rhs) = (ns2, (tag, args', rhs'))
  where
    (ns1, args', env') = newBinder ns args
    (ns2, rhs') = renameGenExpr newBinder (env' ++ env) ns1 rhs

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
