import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hw1 (toDigits, toDigitsRev, doubleEveryOther)

main :: IO ()
main = do
  defaultMain homework

homework =
  testGroup
    "Homework tests"
    [round1]

round1 =
  testGroup
    "Round 1"
    [ex1, ex2]

ex1 =
  testGroup
    "Exercise 1"
    [toDigitsTests, toDigitsRevTests]

toDigitsTests =
  testGroup
    "toDigits"
    [zeroReturnsNull, negativeReturnsNull, intReturnsDigits]

toDigitsRevTests =
  testGroup
    "toDigitsRev"
    [listIsReversed]

zeroReturnsNull = testCase "Zero returns null"
  (assertEqual "Should return null" [] (toDigits 0))

negativeReturnsNull = testCase "Negative returns null"
  (assertEqual "Should return null" [] (toDigits (-1)))

intReturnsDigits = testCase "Int returns list of digits"
  (assertEqual "Should return list of digits" [1, 2, 3] (toDigits 123))

listIsReversed = testCase "List is reversed"
  (assertEqual "Should reverse list" [1, 2, 3] (toDigitsRev 321))

ex2 =
  testGroup
    "Exercise 2"
    [doubleEveryOtherTests]

doubleEveryOtherTests =
  testGroup
    "doubleEveryOther"
    [doublesSecondAndFourth, doublesSecond]

doublesSecondAndFourth = testCase "Second and fourth Integer is doubled"
  (assertEqual "Should return list of Integers" [16, 7, 12, 5] (doubleEveryOther [8, 7, 6, 5]))

doublesSecond = testCase "Second Integer is doubled"
  (assertEqual "Should return list of Integers" [1, 2, 3] (doubleEveryOther [1, 4, 3]))

