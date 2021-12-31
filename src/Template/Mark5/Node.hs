module Template.Mark5.Node where

import Language
import Heap ( Addr )
import Template.Mark5.Primitive

data Node
    = NAp Addr Addr
    | NSupercomb Name [Name] CoreExpr
    | NNum Int
    | NInd Addr
    | NPrim Name Primitive
    | NData Tag [Addr]
    deriving Show

dispatchNode :: (Addr -> Addr -> a)               -- ^ NAp
             -> (Name -> [Name] -> CoreExpr -> a) -- ^ NSupercomb
             -> (Int -> a)                        -- ^ NInt
             -> (Addr -> a)                       -- ^ NInd
             -> (Name -> Primitive -> a)          -- ^ NPrim
             -> (Tag -> [Addr] -> a)              -- ^ NData
             -> Node -> a
dispatchNode nap nsupercomb nnum nind nprim ndata node = case node of
    NAp a b                -> nap a b
    NSupercomb f args body -> nsupercomb f args body
    NNum n                 -> nnum n
    NInd a                 -> nind a
    NPrim name prim        -> nprim name prim
    NData tag contents     -> ndata tag contents
