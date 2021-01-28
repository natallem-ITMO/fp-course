{-# LANGUAGE LambdaCase #-}

module Block2.Task2Spec
  (
    spec
  ) where

import Data.List.NonEmpty (NonEmpty (..))

import Block2.Task2 (joinWith, splitOn)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "splitOn letters" $ do
    it "Empty Input" $ do
      splitOn '\\' "" `shouldBe` listEmptyString
      splitOn '/' "" `shouldBe` listEmptyString
      splitOn 'a' "" `shouldBe` listEmptyString
      splitOn '8' "" `shouldBe` listEmptyString
    it "one letter" $ do
      splitOn 'a' "a" `shouldBe` ("" :| [""])
      splitOn 'b' "a" `shouldBe` listOneString "a"
      splitOn '\\' "\\" `shouldBe` ("" :| [""])
      splitOn '\\' "d" `shouldBe` listOneString "d"
    it "two letters" $ do
      splitOn 'a' "aa" `shouldBe` ("" :| ["", ""])
      splitOn 'a' "ab" `shouldBe` ("" :| ["b"])
      splitOn 'a' "ba" `shouldBe` ("b" :| [""])
      splitOn 'a' "bc" `shouldBe` listOneString "bc"
    it "three letters" $ do
      splitOn 'a' "aaa" `shouldBe` ("" :| ["", "", ""])
      splitOn 'a' "aab" `shouldBe` ("" :| ["", "b"])
      splitOn 'a' "aba" `shouldBe` ("" :| ["b", ""])
      splitOn 'a' "baa" `shouldBe` ("b" :| ["", ""])
      splitOn 'a' "bab" `shouldBe` ("b" :| ["b"])
      splitOn 'a' "bbb" `shouldBe` listOneString "bbb"
    it "many letters" $ do
      splitOn 'a' "aaaaa" `shouldBe` ("" :| ["", "", "", "", ""])
      splitOn 'a' "aasdsfaaab" `shouldBe` ("" :| ["", "sdsf", "", "", "b"])
      splitOn 'a' "aasdsfaslfdjlkgjfaab" `shouldBe` ("" :| ["", "sdsf", "slfdjlkgjf", "", "b"])
  describe "splitOn blanks" $ do
    it "one blank" $ do
      splitOn 'd' " " `shouldBe` listOneString " "
      splitOn 'd' " " `shouldBe` listOneString " "
      splitOn '\\' " " `shouldBe` listOneString " "
      splitOn ' ' " " `shouldBe` ("" :| [""])
      splitOn ' ' "b" `shouldBe` listOneString "b"
    it "more blacks" $ do
      splitOn 'a' " " `shouldBe` (" " :| [])
      splitOn 'a' "     " `shouldBe` ("     " :| [])
      splitOn ' ' "     " `shouldBe` ("" :| ["", "", "", "", ""])
      splitOn ' ' "hello world" `shouldBe` ("hello" :| ["world"])
      splitOn ' ' "hello world " `shouldBe` ("hello" :| ["world", ""])
      splitOn ' ' " hello world " `shouldBe` ("" :| ["hello", "world", ""])
      splitOn 'e' "       hello world " `shouldBe` ("       h" :| ["llo world "])
      splitOn 'e' "       hello     worlde     " `shouldBe` ("       h" :| ["llo     world", "     "])
  describe "hard modification" $ do
    it "joinWith" $ do
      joinWith '4' ("" :| []) `shouldBe` ""
      joinWith '4' (" " :| []) `shouldBe` " "
      joinWith '2' (" " :| [" "]) `shouldBe` " 2 "
      joinWith '2' (" " :| [" ", " "]) `shouldBe` " 2 2 "
      joinWith '8' (" " :| [" ", " hj "]) `shouldBe` " 8 8 hj "
      joinWith '2' (" " :| [" ", " ", "222"]) `shouldBe` " 2 2 2222"
      joinWith ' ' ("hop" :| ["la", "la", "ley"]) `shouldBe` "hop la la ley"
      joinWith ' ' ("I" :| ["like", "haskell", ":)"]) `shouldBe` "I like haskell :)"
    it "joinWith x . splitOn x" $ do
      joinSplitComposition ' ' "   " `shouldBe` "   "
      joinSplitComposition ' ' "" `shouldBe` ""
      joinSplitComposition ' ' "fdsjlkdf    " `shouldBe` "fdsjlkdf    "
      joinSplitComposition ' ' "  fdsjlkdf    " `shouldBe` "  fdsjlkdf    "
      joinSplitComposition ' ' "  fdsj  lkdf    " `shouldBe` "  fdsj  lkdf    "
      joinSplitComposition '3' "33333" `shouldBe` "33333"
      joinSplitComposition '3' "5555" `shouldBe` "5555"
      joinSplitComposition '4' "04.10.2020" `shouldBe` "04.10.2020"
      joinSplitComposition '.' "04.10.2020" `shouldBe` "04.10.2020"
      joinSplitComposition '!' "04.10.2020" `shouldBe` "04.10.2020"

-- | Return NonEmpty list,
--  containing only ""
listEmptyString :: NonEmpty [Char]
listEmptyString = listOneString ""

-- | Return NonEmpty list of strings, in
-- which only first element(= argument string)
listOneString :: [Char] -> NonEmpty [Char]
listOneString str = str :| []

-- | Function to check task restriction
joinSplitComposition :: Char -> String -> String
joinSplitComposition x = joinWith x . splitOn x
