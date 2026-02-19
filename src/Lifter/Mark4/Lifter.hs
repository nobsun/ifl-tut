-- # Lifter.Mark4.Lifter
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lifter.Mark4.Lifter
    where

import Control.Arrow
import Data.List
import Data.Set qualified as S
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as F
import Data.Functor.Foldable
import Debug.Trace
import Language hiding (pprint, )
import Lambda
import Utils
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.PPrint
import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract
import Lifter.Mark4.FullLazy
import Lifter.Mark3.Lifter (lambdaLiftJ)

run :: String -> ([String] -> [String])
run prog inputs 
    = Gm7.showFullResults 
    $ Gm7.eval 
    $ Gm7.setControl inputs
    $ Gm7.compile
    $ lambdaLift
    $ fullyLazyLift
    $ parse prog

sample0, sample1, sample2 :: IO String
sample0 = readFile "prog/lift4/sample0.ifl"
sample1 = readFile "prog/lift4/sample1.ifl"
sample2 = readFile "prog/lift4/sample2.ifl"

{-
-}

check :: (CoreProgram -> CoreProgram) -> String -> IO ()
check f = putStrLn . pprint . f . parse

checkGen :: Show a => (CoreProgram -> Program a) -> String -> IO ()
checkGen f = putStrLn . pprintGen def . f . parse
    where
        def = iStr . show

checkAnnGen :: (Show a, Show ann )
            => (CoreProgram -> AnnProgram a ann) -> String -> IO ()
checkAnnGen f = putStrLn . pprintAnn def defann . f . parse
    where
        def = iStr . show
        defann = iStr . show

runF :: String -> String
runF = pprint . lambdaLift . fullyLazyLift . parse

runS :: String -> String
runS = pprint . lambdaLift . parse

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars . elimLambda

elimLambda :: CoreProgram -> CoreProgram
elimLambda = map elim
    where
        elim :: CoreScDefn -> CoreScDefn
        elim = \ case
            (sc, args0 , ELam args body) -> (sc, args0 ++ args, body)
            scdefn                       -> scdefn

sampleMini :: IO String
sampleMini = return "g y = a (m x x) y"

hoge :: Int -> Int
hoge x
    = let
        g = \ y -> (x * x) + y
      in g 3 + g 4

hugo :: Int -> Int
hugo x 
    = let
        g = huga x
      in  let
            v = g 3 + g 4
          in v

huga :: Int -> Int -> Int
huga x y 
    = let
        v = (+) (x * x)
      in  v y

huge :: Int -> Int
huge x
    = let
        u = x * x
      in  let
            g = hage u
          in g 3 + g 4

hage :: Int -> Int -> Int
hage u y = u + y

tracing :: Show a => a -> a
tracing a = trace (show a) a

{-
変換前
f x = let
        g = \ y -> x * x + y
      in g 3 + g 4
main = f 6

変換後
f x = let
        g = sc x
      in let
           v = g 3 + g 4
         in v

sc x y = let
           v = (+) (x * x) 
         in
           v y
main = f 6
-}