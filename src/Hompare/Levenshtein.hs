-----------------------------------------------------------------------------
--
-- Module      :  Levenshtein
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

module Hompare.Levenshtein (
    levenshtein
) where

-- Levenshtein distance between two strings
levenshtein :: String -> String -> Int
levenshtein "" "" = 0
levenshtein s1 "" = length s1
levenshtein "" s2 = length s2
levenshtein s t = d !! (length s) !! (length t)
    where
        d = [[distance m n | n <- [0..length t]] | m <- [0..length s]]
        distance i 0 = i
        distance 0 j = j
        distance i j =
            minimum [d !! (i - 1) !! j + 1,
                     d !! i !! (j - 1) + 1,
                     d !! (i - 1) !! (j - 1) + (if s !! (i - 1) == t !! (j - 1) then 0 else 1)]
