module Hw1
    ( toDigits,
      toDigitsRev,
      doubleEveryOther
    ) where

-- exercise 1

toDigits    :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise = map (read . (:[])) . show $ i

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = xs
