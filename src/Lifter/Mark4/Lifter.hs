-- # Lifter.Mark4.Lifter
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
module Lifter.Mark4.Lifter
    where

import Language
import Parse
import Pretty
import Iseq

import Gmachine.Mark7.Machine qualified as Gm7
import Gmachine.Mark7.Compiler qualified as Gm7
import Gmachine.Mark7.PPrint  qualified as Gm7

import Lifter.FreeVars
import Lifter.Mark4.Rename
import Lifter.Mark4.Collect
import Lifter.Mark4.Abstract
import Lifter.Mark4.SeparateLambda
import Lifter.Mark4.AddLevel
import Lifter.Mark4.IdentifyMFE
import Lifter.Mark4.Floating
import Lifter.Mark4.FullLazy
import Lifter.Mark4.LambdaLift

run :: String -> ([String] -> [String])
run prog inputs 
    = Gm7.showFullResults 
    $ Gm7.eval 
    $ Gm7.setControl inputs
    $ Gm7.compile
    $ lambdaLift
    $ fullyLazyLift
    $ parse prog

runF :: String -> String
runF = pprint . fullyLazyLift . parse

runS :: String -> String
runS = pprint . lambdaLift . parse

{- test -}
sample :: Int -> IO String
sample = \ case
    0 -> readFile "prog/lift4/sample661.ifl"
    _ -> readFile "prog/lift4/sample672.ifl"

{- |
>>> runPrintSource 0
f x = let
          g = (x * x +)
      in
          g 3 + g 4;
main = f 6
>>> runSeparateLams 0
f = λ x → let
        g = (x * x +)
    in
        g 3 + g 4;
main = f 6
>>> runLambdaLiftFullyLazyLift 0
f x_0_1 = let
              v_3_6 = x_0_1 * x_0_1
          in
              let
                  g_1_2 = sc_3 v_3_6
              in
                  g_1_2 3 + g_1_2 4;
sc_3 v_3_4 y_2_5 = v_3_4 + y_2_5;
main = f 6
>>> runFreeVarsSeparateLams 0
f = ⟪fromList [],λ "x" → ⟪fromList ["x"],let
        "g" = ⟪fromList ["x"],λ "y" → ⟪fromList ["x","y"],⟪fromList ["x"],⟪fromList ["x"],⟪fromList ["x"],⟪fromList ["x"],x⟫ ⟪fromList [],*⟫⟫ ⟪fromList ["x"],x⟫⟫ ⟪fromList [],+⟫⟫ ⟪fromList ["y"],y⟫⟫⟫
    in
        ⟪fromList ["g"],⟪fromList ["g"],⟪fromList ["g"],⟪fromList ["g"],g⟫ ⟪fromList [],3⟫⟫ ⟪fromList [],+⟫⟫ ⟪fromList ["g"],⟪fromList ["g"],g⟫ ⟪fromList [],4⟫⟫⟫⟫⟫;
main = ⟪fromList [],⟪fromList [],f⟫ ⟪fromList [],6⟫⟫
>>> runAddLevelsSeparateLams 0
f = ⟪0,λ ("x",1) → ⟪1,let
        ("g",1) = ⟪1,λ ("y",2) → ⟪2,⟪2,⟪1,⟪1,⟪1,x⟫ ⟪1,*⟫⟫ ⟪1,x⟫⟫ ⟪1,+⟫⟫ ⟪2,y⟫⟫⟫
    in
        ⟪1,⟪1,⟪1,⟪1,g⟫ ⟪1,3⟫⟫ ⟪1,+⟫⟫ ⟪1,⟪1,g⟫ ⟪1,4⟫⟫⟫⟫⟫;
main = ⟪0,⟪0,f⟫ ⟪0,6⟫⟫
>>> runIdentifyMFEsAddLevelsSeparateLams 0
f = λ ("x",1) → let
        ("g",1) = ((let
            ("v",1) = x * x
        in
            v) +)
    in
        g 3 + g 4;
main = f 6
>>> runRenameLIdentifyMFEsAddLevelsSeparateLams 0
f = λ ("x_0",1) → let
        ("g_1",1) = ((let
            ("v_3",1) = x_0 * x_0
        in
            v_3) +)
    in
        g_1 3 + g_1 4;
main = f 6
>>> runFullyLazyLift 0
f = λ x_0 → let
        v_3 = x_0 * x_0
    in
        let
            g_1 = (v_3 +)
        in
            g_1 3 + g_1 4;
main = f 6
>>> runFreeVarsFullyLazyLift 0
f = ⟪fromList [],λ "x_0" → ⟪fromList ["x_0"],let
        "v_3" = ⟪fromList ["x_0"],⟪fromList ["x_0"],⟪fromList ["x_0"],x_0⟫ ⟪fromList [],*⟫⟫ ⟪fromList ["x_0"],x_0⟫⟫
    in
        ⟪fromList ["v_3"],let
            "g_1" = ⟪fromList ["v_3"],λ "y_2" → ⟪fromList ["v_3","y_2"],⟪fromList ["v_3"],⟪fromList ["v_3"],v_3⟫ ⟪fromList [],+⟫⟫ ⟪fromList ["y_2"],y_2⟫⟫⟫
        in
            ⟪fromList ["g_1"],⟪fromList ["g_1"],⟪fromList ["g_1"],⟪fromList ["g_1"],g_1⟫ ⟪fromList [],3⟫⟫ ⟪fromList [],+⟫⟫ ⟪fromList ["g_1"],⟪fromList ["g_1"],g_1⟫ ⟪fromList [],4⟫⟫⟫⟫⟫⟫;
main = ⟪fromList [],⟪fromList [],f⟫ ⟪fromList [],6⟫⟫
>>> runAbstractFreeVarsFullyLazyLift 0
f = let
        sc = λ x_0 → let
            v_3 = x_0 * x_0
        in
            let
                g_1 = (let
                    sc = (+)
                in
                    sc) v_3
            in
                g_1 3 + g_1 4
    in
        sc;
main = f 6
>>> runRenameAbstractFreeVarsFullyLazyLift 0
f = let
        sc_0 = λ x_0_1 → let
            v_3_6 = x_0_1 * x_0_1
        in
            let
                g_1_2 = (let
                    sc_3 = (+)
                in
                    sc_3) v_3_6
            in
                g_1_2 3 + g_1_2 4
    in
        sc_0;
main = f 6
>>> runLambdaLiftFullyLazyLift 0
f x = let
          v = x * x
      in
          let
              g = sc v
          in
              g 3 + g 4;
sc v y = v + y;
main = f 6
-}

runPrintSource :: Int -> IO ()
runPrintSource = (check id =<<) . sample

runSeparateLams :: Int -> IO ()
runSeparateLams = (check separateLams =<<) . sample

runFreeVarsSeparateLams :: Int -> IO ()
runFreeVarsSeparateLams
    = (checkAnnGen (freeVars . separateLams) =<<) . sample

runAddLevelsSeparateLams :: Int -> IO ()
runAddLevelsSeparateLams
    = (checkAnnGen (freeToLevel . freeVars . separateLams) =<<) . sample

runIdentifyMFEsAddLevelsSeparateLams :: Int -> IO ()
runIdentifyMFEsAddLevelsSeparateLams
    = (checkGen (identifyMFEs . addLevels . separateLams) =<<) . sample

runRenameLIdentifyMFEsAddLevelsSeparateLams :: Int -> IO ()
runRenameLIdentifyMFEsAddLevelsSeparateLams
    = (checkGen (renameL . identifyMFEs . addLevels . separateLams) =<<) . sample

runFullyLazyLift :: Int -> IO ()
runFullyLazyLift
    = (check (float . renameL . identifyMFEs . addLevels . separateLams) =<<) . sample

runFreeVarsFullyLazyLift :: Int -> IO ()
runFreeVarsFullyLazyLift
    = (checkAnnGen (freeVars . fullyLazyLift) =<<) . sample

runAbstractFreeVarsFullyLazyLift :: Int -> IO ()
runAbstractFreeVarsFullyLazyLift
    = (check (abstract . freeVars . fullyLazyLift) =<<) . sample

runRenameAbstractFreeVarsFullyLazyLift :: Int -> IO ()
runRenameAbstractFreeVarsFullyLazyLift
    = (check (rename . abstract . freeVars . fullyLazyLift) =<<) . sample

runLambdaLiftFullyLazyLift :: Int -> IO ()
runLambdaLiftFullyLazyLift 
    = (check (collectSCs . rename . abstract . freeVars . fullyLazyLift) =<<) . sample

check :: (CoreProgram -> CoreProgram) -> String -> IO ()
check f = putStrLn . pprint . f . parse

checkGen :: (VarRep a, Show a) => (CoreProgram -> Program a) -> String -> IO ()
checkGen f = putStrLn . pprintGen def . f . parse
    where
        def = iStr . show

checkAnnGen :: (VarRep a, Show a, Show ann )
            => (CoreProgram -> AnnProgram a ann) -> String -> IO ()
checkAnnGen f = putStrLn . pprintAnn def defann . f . parse
    where
        def = iStr . show
        defann = iStr . show
