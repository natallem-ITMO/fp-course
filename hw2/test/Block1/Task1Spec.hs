module Block1.Task1Spec
  (
     spec
  )  where

import Data.Maybe (fromMaybe)

import Block1.Task1 (stringSum)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "stringSum" $ do
    it "one correct number" $ do
      stringSum "1" `shouldBe` Just 1
      stringSum "1        " `shouldBe` Just 1
      stringSum "          1" `shouldBe` Just 1
      stringSum "\n\n1" `shouldBe` Just 1
      stringSum "\r\r\r1" `shouldBe` Just 1
      stringSum "1\r\r\n\n    " `shouldBe` Just 1
      stringSum "\n\n1\r\r\n\n    " `shouldBe` Just 1
      stringSum "              \n\n1" `shouldBe` Just 1
    it "couple correct numbers" $ do
      stringSum "1 2 3" `shouldBe` Just 6
      stringSum "1     2 3" `shouldBe` Just 6
      stringSum "1     2 3       " `shouldBe` Just 6
      stringSum "1     2\n\n3       " `shouldBe` Just 6
      stringSum "\n1     2\n\n3       " `shouldBe` Just 6
      stringSum "    1     2\n\n3       " `shouldBe` Just 6
      stringSum "    1     2\n\n-3       " `shouldBe` Just 0
      stringSum "    -1     2\n\n-3       " `shouldBe` Just (-2)
      stringSum "    -1     2\n\n-3       -323" `shouldBe` Just (-325)
    it "incorrect input" $ do
      stringSum "1 4r 5" `shouldBe` Nothing
      stringSum "1 4.3 5" `shouldBe` Nothing
      stringSum "1.3" `shouldBe` Nothing
      stringSum "1!" `shouldBe` Nothing
      stringSum "1~" `shouldBe` Nothing
      stringSum "1 2 3 55 6 7)" `shouldBe` Nothing
      stringSum "hello" `shouldBe` Nothing
      stringSum "hi" `shouldBe` Nothing
  describe "property tests" $ do
    modifyMaxSuccess (const 1000) $
      it
        "random blanks between numbers"
        $ property randomCheck

-- | Construct string of random numbers separated by random number of blank char
randomCheck :: [(Int, Int)] -> Bool
randomCheck xs =
  fromMaybe (sumList + 1) (stringSum separatedNumsInString) == sumList
  where
    sumList = sum $ map fst xs
    separatedNumsInString =
      concatMap
        ( \(x, y) ->
            (show x) ++ (replicate (abs y + 1) ' ')
        )
        xs
