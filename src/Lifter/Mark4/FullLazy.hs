-- # Lifter.Mark4.FullLazy
{-# LANGUAGE GHC2024 #-}
module Lifter.Mark4.FullLazy
    where

import Language
import Lifter.Mark4.Rename
import Lifter.Mark4.Floating
import Lifter.Mark4.IdentifyMFE
import Lifter.Mark4.SeparateLambda
import Lifter.Mark4.AddLevel

fullyLazyLift :: CoreProgram -> CoreProgram
fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
