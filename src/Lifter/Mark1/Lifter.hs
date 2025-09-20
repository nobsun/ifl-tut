-- # Lifter.Mark1.Lifter
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark1.Lifter
    where

import Data.Char
import Data.Function
import Data.List
-- import Text.ParserCombinators.ReadP

import Language
import Heap
import qualified Stack as Stk (push, pop, npop, discard)
import Stack hiding (push, pop, npop, discard)
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

run :: String -> ([String] -> [String])
run prog inputs 
    = Gm7.showFullResults 
    $ Gm7.eval 
    $ Gm7.setControl inputs
    $ Gm7.compile
    $ lambdaLifter
    $ parse prog

lambdaLifter :: CoreProgram -> CoreProgram
lambdaLifter = id
