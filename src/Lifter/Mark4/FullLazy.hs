-- # Lifter.Mark4.FullLazy
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Lifter.Mark4.FullLazy
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable

import Language
import Lambda
import Pretty
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract
import Lifter.Mark4.Floating
import Lifter.Mark4.IdentifyMFE
import Lifter.Mark4.SeparateLambda
import Lifter.Mark4.AddLevel

fullyLazyLift :: CoreProgram -> CoreProgram
fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
