module Main
where

import Text.Printf
import Control.Exception
import Control.Concurrent (threadDelay)
import System.CPUTime
import Hompare

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    --threadDelay 1000000
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %d - %d = %0.3f sec\n" end start (diff :: Double)
    return v

main = do
	time $ print $ jaroWinkler "MARTHA" "MARHTA"
	time $ print $ jaroWinkler "DIXON" "DICKSONX"
	time $ print $ cologneMethod ""
	time $ print $ cologneMethod "Wikipedia"
	time $ print $ cologneMethod "Muller-Ludenscheidt"
	
