-- # Lifter.Mark4.Lifter
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Lifter
    where

import Language

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.Mark3.Rename
import Lifter.Mark3.Collect
import Lifter.Mark3.FreeVars
import Lifter.Mark3.Abstract

run :: String -> ([String] -> [String])
run prog inputs 
    = Gm7.showFullResults 
    $ Gm7.eval 
    $ Gm7.setControl inputs
    $ Gm7.compile
    $ lambdaLiftJ
    $ parse prog

runJ :: String -> String
runJ = pprint . lambdaLiftJ . parse

lambdaLiftJ :: CoreProgram -> CoreProgram
lambdaLiftJ = collectSCs . abstractJ . freeVars . rename
