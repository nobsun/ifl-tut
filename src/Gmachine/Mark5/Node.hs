{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark5.Node
    where

import Heap ( Addr )
import Gmachine.Mark5.Code

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
    | NInd Addr
    deriving (Eq, Show)
