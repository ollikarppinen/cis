module Hw1
    ( toDigits,
      toDigitsRev
    ) where

-- exercise 1

toDigits    :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise = map (read . (:[])) . show $ i

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
