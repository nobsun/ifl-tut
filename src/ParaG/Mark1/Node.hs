{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ParaG.Mark1.Node
    where

import Heap ( Addr )
import Language
import ParaG.Mark1.Code

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
    | NInd Addr
    | NConstr Tag [Addr]
    deriving (Eq, Show, Read)
