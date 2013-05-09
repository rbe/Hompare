-----------------------------------------------------------------------------
--
-- Module      :  Jaro
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Hompare.Jaro (
    jaro
    , winkler
    , jaroWinkler
) where

import Hompare.Helper

-- Compute Jaro distance between two strings
jaro :: String -> String -> Double
jaro "" _ = 0.0
jaro _ "" = 0.0
jaro s1 s2
    | s1 == s2 = 1.0
    | s1 /= s2 =
        let
            -- Length of both strings
            l1 = length s1
            l2 = length s2
            -- Index s2
            z2 = zip [1..] s2
            m = foldl (++) [] [charMatch p ((max l1 l2) `div` 2) z2 | p <- zip [1..] s1]
            ml = length m
            t = sum [realToFrac (transposition p z2) / 2.0 | p <- m]
            ml1 = realToFrac ml / realToFrac l1
            ml2 = realToFrac ml / realToFrac l2
            mtm = (realToFrac ml - t) / realToFrac ml
        in
            (1 / 3) * (ml1 + ml2 + mtm)
        where
            -- [] of matching characters for 1 character
            charMatch (p,q) far list = filter (\(x,y) -> x >= p - far && x <= p + far && y == q) list
            -- # of transpositions for 1 character
            transposition (p,q) list = length $ filter (\(x,y) -> p /= x && q == y) list

-- Compute Winkler distance between two strings on top of Jaro distance
winkler :: String -> String -> Double -> Double
winkler "" _ _ = 0.0
winkler _ "" _ = 0.0
winkler s1 s2 jaro
    | s1 == s2 = 1.0
    | s1 /= s2 =
        let
            l = length $ commonPrefix s1 s2
            p = 0.1
        in
            jaro + ((realToFrac l * p) * (1.0 - jaro))

-- Compute Jaro-Winkler distance between two strings
jaroWinkler :: String -> String -> Double
jaroWinkler "" _ = 0.0
jaroWinkler _ "" = 0.0
jaroWinkler s1 s2
    | s1 == s2 = 1.0
    | s1 /= s2 = winkler s1 s2 $ jaro s1 s2
