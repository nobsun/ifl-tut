-- # Lifter.Mark4.LambdaLift
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.LambdaLift
    where

import Language
import Lifter.FreeVars
import Lifter.Mark4.Abstract
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

