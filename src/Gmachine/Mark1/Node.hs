{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Gmachine.Mark1.Node
    where

import Language
import Heap ( Addr )
import Utils 
import Gmachine.Mark1.Code

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
