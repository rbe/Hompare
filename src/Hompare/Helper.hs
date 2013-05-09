-----------------------------------------------------------------------------
--
-- Module      :  Helper
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

module Hompare.Helper (
    intListToInt
    , commonPrefix
) where

-- Convert list of integer to one integer
intListToInt :: [Integer] -> Integer
--intListToInt = foldr (+) 0 . map (\(x,y) -> y * (10 ^ x `div` 10)) . zip [1..] . reverse
intListToInt = foldl (\a x -> x + a * 10) 0

-- Common prefix of two strings; up to 4 characters
commonPrefix :: String -> String -> String
commonPrefix "" "" = ""
commonPrefix "" _ = ""
commonPrefix _ "" = ""
commonPrefix s1 s2
    | s1 == s2 = take 4 s1
    | otherwise = cp s1 s2 []
    where
        cp s1 s2 acc
            | length acc == 4 = acc
            | head s1 == head s2 = head s1 : cp (tail s1) (tail s2) acc
            | otherwise = acc
