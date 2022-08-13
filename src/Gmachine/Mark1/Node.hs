{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark1.Node
    where

import Heap ( Addr )
import Gmachine.Mark1.Code

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
