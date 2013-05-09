-----------------------------------------------------------------------------
--
-- Module      :  CologneMethod
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

module Hompare.CologneMethod (
    cologneMethod
) where

import Data.Char (toUpper)
import Hompare.Helper

-- Cologne Method: compute 'Kölner Verfahren' for a string
cologneMethod :: String -> Integer
cologneMethod "" = 0
cologneMethod s = intListToInt $ foldr cons [] $ filter (> 0) $ cologne [toUpper x | x <- s] '\NUL'
    where
        -- Construct list of Ints while adjacent elements would never be the same
        cons :: Integer -> [Integer] -> [Integer]
        cons x [] = [x]
        cons x acc
            | x == head acc = acc
            | otherwise = x:acc
        --
        cologne :: String -> Char -> [Integer]
        cologne "" _ = [0]
        cologne s p = cm (head s) p (s !! 1) : cologne (tail s) (head s)
        -- Return code for character
        cm :: Char -> Char -> Char -> Integer
        -- TODO UTF-8 cm 'Ä' p n = cm 'AE' p n
        -- TODO UTF-8 cm 'Ö' p n = cm 'OE' p n
        -- TODO UTF-8 cm 'Ü' p n = cm 'UE' p n
        -- TODO UTF-8 cm 'ß' p n = cm 'SS' p n
        cm c p n
            | c `elem` "AEIJOUY" = 0
            | c == 'H' = 0
            | c == 'B' = 1
            | c == 'P' && n /= 'H' = 1
            | c `elem` "DT" && not (p `elem` "CSZ") = 2
            | c `elem` "FVW" = 3
            | c == 'P' && n == 'H' = 3
            | c `elem` "GKQ" = 4
            | c == 'C' && p == '\NUL' && n `elem` "AHKLOQRUX" = 4
            | c == 'C' && p /= '\NUL' && n `elem` "AHKOQUX" && not (p `elem` "SZ") = 4
            | c == 'X' && not (n `elem` "CKQ") = 48
            | c == 'L' = 5
            | c `elem` "MN" = 6
            | c == 'R' = 7
            | c `elem` "SZ" = 8
            | c == 'C' && n `elem` "SZ" = 8
            | c == 'C' && p == '\NUL' && not (n `elem` "AHKLOQRUX") = 8
            | c == 'C' && not (n `elem` "AHKOQUX") = 8
            | c `elem` "DT" && p `elem` "CSZ" = 8
            | c == 'X' && n `elem` "CKQ" = 8
            | otherwise = 0
