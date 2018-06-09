module Hw1
    ( toDigits,
      toDigitsRev,
      doubleEveryOther,
      sumDigits
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
doubleEveryOther xs = map doubleIfEven $ zip xs [0..]

doubleIfEven :: (Integer, Integer) -> Integer
doubleIfEven x
  | even $ fst $ x = fst x * 2
  | otherwise     = fst x

-- exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

