import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hw1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate, hanoi)
import LogAnalysis (parseMessage, insert, build, inOrder, whatWentWrong)
import Log
import Golf (skips)

main :: IO ()
main = do
  defaultMain homework

homework =
  testGroup
    "Homework tests"
    [round1, round2, round3]

round1 =
  testGroup
    "Round 1"
    [ex1, ex2, ex3, ex4, ex5]

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
  (assertEqual "Should return list of Integers" [2, 2, 6] (doubleEveryOther [1, 2, 3]))

ex3 =
  testGroup
    "Exercise 3"
    [sumDigitsTests]

sumDigitsTests =
  testGroup
    "sumDigits"
    [sumsIntegerArrayDigits]

sumsIntegerArrayDigits = testCase "Integer array digits are summed"
  (assertEqual "Should return sum of digits" 22 (sumDigits [16, 7, 12, 5]))

ex4 =
  testGroup
    "Exercise 4"
    [validateTests]

validateTests =
  testGroup
    "validate"
    [validatesCorrect, validatesIncorrect]

validatesCorrect = testCase "Validates valid credit card when correct"
  (assertEqual "Should validate credit card" True (validate 4012888888881881))

validatesIncorrect = testCase "Validates valid credit card when incorrect"
  (assertEqual "Should validate credit card" False (validate 4012888888881882))

ex5 =
  testGroup
    "Exercise 5"
    [hanoiTests]

hanoiTests =
  testGroup
    "hanoi"
    [hanoiMovesStackToSecondPeg]

hanoiMovesStackToSecondPeg = testCase "Moves stack to second peg"
  (assertEqual "Should return a correct sequence of moves" [("a","c"), ("a","b"), ("c","b")] (hanoi 2 "a" "b" "c"))

round2 =
  testGroup
    "Round 2"
    [r2ex1, r2ex2]

r2ex1 =
  testGroup
    "Exercise 1"
    [parseMessageTest]

parseMessageTest =
  testGroup
    "parseMessage"
    [parseError, parseInfo, parseUnknown]

errorMessage = LogMessage (Error 2) 562 "help help"
infoMessage = LogMessage Info 29 "la la la"
unknownMessage = Unknown "This is not in the right format"

parseError = testCase "Parses Error"
  (assertEqual "Should parse Error correctly" errorMessage (parseMessage "E 2 562 help help"))

parseInfo = testCase "Parses Info"
  (assertEqual "Should parse Info correctly" infoMessage (parseMessage "I 29 la la la"))

parseUnknown = testCase "Parses Unknown"
  (assertEqual "Should parse Unknown correctly" unknownMessage (parseMessage "This is not in the right format"))

r2ex2 =
  testGroup
    "Exercise 2"
    [insertMessageTest, buildMessagesTest]

insertMessageTest =
  testGroup
    "InsertMessage"
    [insertError, insertInfo, insertUnknown, insertLessThan, insertGreaterThan]

insertError = testCase "Inserts Error"
  (assertEqual "Should insert Error" (Node Leaf errorMessage Leaf) (insert errorMessage Leaf))

insertInfo = testCase "Inserts Info"
  (assertEqual "Should insert Info" (Node Leaf infoMessage Leaf) (insert infoMessage Leaf))

insertUnknown = testCase "Inserts Unknown"
  (assertEqual "Should not insert Unknown" Leaf (insert unknownMessage Leaf))

insertLessThan = testCase "Inserts less than timestamps"
  (assertEqual "Should insert into left tree" (Node (Node Leaf infoMessage Leaf) errorMessage Leaf) (insert infoMessage (Node Leaf errorMessage Leaf)))

insertGreaterThan = testCase "Inserts greater than timestamps"
  (assertEqual "Should insert into right tree" (Node Leaf infoMessage (Node Leaf errorMessage Leaf)) (insert errorMessage (Node Leaf infoMessage Leaf)))

buildMessagesTest =
  testGroup
    "BuildMessages"
    [buildMessages]

buildMessages = testCase "Builds messages"
  (assertEqual "Should build message tree" (Node (Node Leaf infoMessage Leaf) errorMessage Leaf) (build [infoMessage, errorMessage, unknownMessage]))

inOrderMessagesTest =
  testGroup
    "InOrderMessages"
    [inOrderMessages]

m1 = LogMessage Info 1 ""
m2 = LogMessage Info 2 ""
m3 = LogMessage Info 3 ""
m4 = LogMessage Info 4 ""
inOrderMt = Node (Node Leaf m1 (Node Leaf m2 Leaf)) m3 (Node Leaf m4 Leaf)
inOrderMessages = testCase "Returns messages in order"
  (assertEqual "Should return messages in order" [m1, m2, m3, m4] (inOrder inOrderMt))

round3 = testGroup "Round 3" [r3e1]
r3e1 = testGroup "Exercise 1" [skipsStr1, skipsStr2, skipsInt, skipsBool, skipsNone]
skipsStr1 = testCase "Skip string 1" (assertEqual "Returns array" ["ABCD", "BD", "C", "D"] (skips "ABCD"))
skipsStr2 = testCase "Skip string 2" (assertEqual "Returns array" ["hello!", "el!", "l!", "l", "o", "!"] (skips "hello!"))
skipsInt = testCase "Skip integer" (assertEqual "Returns array" [[1 :: Int]] (skips [1]))
skipsBool = testCase "Skip boolean" (assertEqual "Returns array" [[True, False], [False]] (skips [True, False]))
skipsNone = testCase "Skip null" (assertEqual "Returns array" [] (skips ([] :: [Int])))

