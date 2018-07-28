module Golf where

import Data.List

nthEl :: [a] -> Int -> [a]
nthEl a n = case drop (n - 1) a of
  []  -> []
  h:t -> h:(nthEl t n)

skips :: [a] -> [[a]]
skips a = map (nthEl a) [1..length a]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:t) = (if b > a && b > c then [b] else []) ++ localMaxima (b:c:t)
localMaxima _ = []

histogram :: [Integer] -> String
histogram [] = "==========\n0123456789\n"
histogram xs = map(\i -> if i `elem` xs then '*' else ' ') [0..9] ++ "\n" ++ (histogram (xs \\ [0..9]))

