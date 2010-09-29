module Hompare
(
jaroWinkler
, levenshtein
, cologneMethod
)
where

import Prelude
import Data.Char (toUpper)

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
