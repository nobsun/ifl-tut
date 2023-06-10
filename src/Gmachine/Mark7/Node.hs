{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark7.Node
    where

import Heap ( Addr )
import Language
import Gmachine.Mark7.Code

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
    | NInd Addr
    | NConstr Tag [Addr]
    deriving (Eq, Show)
