{-# LANGUAGE CPP, TemplateHaskell #-}
module Main (
    main
) where

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (stripPrefix)
import System.CPUTime
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck.All (quickCheckAll)

import Hompare.Levenshtein
import Hompare.Jaro
import Hompare.CologneMethod

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    --threadDelay 1000000
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %d - %d = %0.3f sec\n" end start (diff :: Double)
    return v

-- exeMain : Executable Entry Point
exeMain = do
    putStrLn "Hompare!"
    time $ print $ jaroWinkler "MARTHA" "MARHTA"
    time $ print $ jaroWinkler "DIXON" "DICKSONX"
    time $ print $ cologneMethod ""
    time $ print $ cologneMethod "Wikipedia"
    printf "Müller-Lüdenscheidt: %d == %d?" (cologneMethod "Müller-Lüdenscheidt") (cologneMethod "Muller-Ludenscheidt")

-- Simple function to create a hello message.
hello s = "Hello " ++ s
prop_helloStripped s = stripPrefix "Hello " (hello s) == Just s

-- Test Kölner Verfahren
prop_cologneMethod1 = do
    let c1 = (cologneMethod "Müller-Lüdenscheidt")
        c2 = (cologneMethod "Muller-Ludenscheidt") in
        c1 == c2

-- testMain : Unit Testing Entry Point
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION
