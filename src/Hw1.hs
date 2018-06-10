module Hw1
    ( toDigits,
      toDigitsRev,
      doubleEveryOther,
      sumDigits,
      validate,
      hanoi
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
  | even $ snd $ x = fst x * 2
  | otherwise     = fst x

-- exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- exercise 4

validate :: Integer -> Bool
validate = isMod10 . sum . (map sum) . (map toDigits) . doubleEveryOther . toDigits

isMod10 :: Integer -> Bool
isMod10 x = rem x 10 == 0

-- exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi h p1 p2 p3
  | h == 1    = [(p1, p2)]
  | otherwise = hanoi (h - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (h - 1) p3 p2 p1

