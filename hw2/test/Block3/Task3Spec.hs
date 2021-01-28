module Block3.Task3Spec
  (
     spec
  )  where

import Block3.Task1 (runParser)
import Block3.Task3 (CBS, correctBracketSequence, integerParser)

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Property, choose, forAll, property)

spec :: Spec
spec = do
  describe "BCS" $ do
    modifyMaxSuccess (const 1000) $
      it
        "correct"
        $ property correctBSTests
    modifyMaxSuccess (const 1000) $
      it
        "incorrect"
        $ property incorrectBSTests
  describe "parsing number" $ do
    modifyMaxSuccess (const 1000) $
      it
        "correct without sign"
        $ property (parseNumberTest id id correctNumbers)
    modifyMaxSuccess (const 1000) $
      it
        "correct with +"
        $ property (parseNumberTest ("+" ++) id correctNumbers)
    modifyMaxSuccess (const 1000) $
      it
        "correct with -"
        $ property (parseNumberTest ("-" ++) negate correctNumbers)
    modifyMaxSuccess (const 1000) $
      it
        "incorrect without sign"
        $ property (parseNumberTest ("djslkj" ++) (const failNumConstant) correctNumbers)
    modifyMaxSuccess (const 1000) $
      it
        "incorrect with +"
        $ property (parseNumberTest ("+re" ++) (const failNumConstant) correctNumbers)
    modifyMaxSuccess (const 1000) $
      it
        "incorrect with -"
        $ property (parseNumberTest ("--" ++) (const failNumConstant) correctNumbers)

-- | Simple property test, that randomly check correct parsing of CBS test example
correctBSTests :: Property
correctBSTests = forAll (choose (1, length correctBSList - 1)) $ \i ->
  let p = correctBSList !! i
   in getFromMaybeCBS (testCBS p) == p

-- | Simple property test, that randomly check incorrect parsing of CBS test example
incorrectBSTests :: Property
incorrectBSTests = forAll (choose (1, length incorrectBSList - 1)) $ \i ->
  let p = incorrectBSList !! i
   in getFromMaybeCBS (testCBS p) == "fail"

-- | Checks parsing of numbers with different
-- leading sign (as '-' / '+' / or empty) using
--  changeInputFunc to add test example sign char
--  and changeResultFunc to change result of test
--  example if negative
parseNumberTest ::
  (String -> String) ->
  (Integer -> Integer) ->
  [(String, Integer)] ->
  Property
parseNumberTest changeInputFunc changeResultFunc list =
  forAll (choose (1, length list - 1)) $ \i ->
    let p = list !! i
     in getFromMaybeNum
          (testParserNum (changeInputFunc (fst p)))
          == changeResultFunc (snd p)


-- | Returns string representation of parsed CBS Maybe obj
-- if it contains value, otherwise return "fail"
getFromMaybeCBS :: Maybe (CBS, String) -> String
getFromMaybeCBS Nothing = "fail"
getFromMaybeCBS (Just (x, "")) = show x
getFromMaybeCBS (Just (_, _)) = "fail"

-- | Returns integer
-- if it contains value, otherwise return "fail"
getFromMaybeNum :: Maybe (Integer, String) -> Integer
getFromMaybeNum Nothing = failNumConstant
getFromMaybeNum (Just (x, _)) = x

failNumConstant :: Integer
failNumConstant = 666

testCBS :: String -> Maybe (CBS, String)
testCBS = runParser correctBracketSequence

testParserNum :: String -> Maybe (Integer, String)
testParserNum = runParser integerParser

correctBSList :: [String]
correctBSList =
  [ "",
    "()",
    "(())",
    "()()",
    "(())()",
    "(())()()",
    "(())()((()))",
    "(())()((()))()()",
    "(())()((()))()(())",
    "()()()()()()()()()()",
    "()(()())()()()()()()()()",
    "()(()())()()()()()()()()",
    "()(()())()()()()(()())()()()",
    "()(()())()()()()()()()()((()))",
    "()(()())()(()()()()())()()()()()()",
    "()(()())()(()()(()()())()())()()()()()()",
    "()(()())()(()()(()(((())))())()())()()()()()()",
    "()(()())()(()()(()(((())))())()())()()((())((())))()()()",
    "()(()())(()()(((())))())(()()(()(((())))())()())()()((())((())))()()()"
  ]

incorrectBSList :: [String]
incorrectBSList =
  [ " ",
    " (",
    " )",
    " () ",
    "  ()()()()",
    "((",
    "( (",
    "(",
    "))",
    ")     )",
    ")",
    ")     ",
    ")  ()()()()(((())))   ",
    "(",
    "((())",
    "((())))",
    "((())))",
    "((())) ()",
    "((()))(()))",
    "((()))))(()))",
    "()(()))))(()))"
  ]

correctNumbers :: [(String, Integer)]
correctNumbers =
  [ ("1", 1),
    ("2", 2),
    ("34", 34),
    ("123432", 123432),
    ("1111", 1111),
    ("11112", 11112),
    ("111124567", 111124567),
    ("0", 0)
  ]
