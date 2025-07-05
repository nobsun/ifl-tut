{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module ParaG.Mark3.Node
    where

import Heap ( Addr )
import Language
import ParaG.Mark3.Code

data Node
    = NNum Int                      -- Numbers
    | NAp Addr Addr                 -- Applications
    | NGlobal Int GmCode            -- Globals
    | NInd Addr                     -- Indirections
    | NConstr Tag [Addr]            -- Constructors
    | NLAp Addr Addr GmTaskId       -- Locked applications
    | NLGlobal Int GmCode GmTaskId  -- Locked globals
    deriving (Eq, Show, Read)

type GmTaskId = Int