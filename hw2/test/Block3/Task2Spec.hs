module Block3.Task2Spec
  (
     spec
  )  where

import Data.Char (isDigit)

import Block3.Task1 (Parser (..), runParser)
import Block3.Task2 (element, eof, ok, satisfy, stream)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "basics" $ do
    it "element" $ do
      runParser
        ( do
            x <- element 's'
            y <- element 'y'
            return [x, y]
        )
        "syf"
        `shouldBe` Just ("sy", "f")
      runParser
        ( do
            x <- element 's'
            y <- element 'y'
            return [x, y]
        )
        "sdf"
        `shouldBe` Nothing
      runParser
        ( do
            x <- element 's'
            y <- element 'y'
            return [x, y]
        )
        "eyf"
        `shouldBe` Nothing
      runParser
        ( do
            x <- element "hello"
            _ <- element "my"
            return x
        )
        ["hello", "my", "name", "is"]
        `shouldBe` Just ("hello", ["name", "is"])
      runParser
        ( do
            x <- element "hello"
            _ <- element "my"
            return x
        )
        ["hello", "name", "is"]
        `shouldBe` Nothing
    it "eof" $ do
      runParser
        ( do
            _ <- satisfy isDigit
            eof
        )
        "3"
        `shouldBe` Just ((), "")
      runParser
        ( do
            _ <- satisfy isDigit
            eof
        )
        "30"
        `shouldBe` Nothing
      runParser
        ( do
            eof
        )
        ""
        `shouldBe` Just ((), "")
      runParser
        ( do
            eof
        )
        "d"
        `shouldBe` Nothing
    it "ok" $ do
      runParser ok "3" `shouldBe` Just ((), "3")
      runParser ok " sdfjlds " `shouldBe` Just ((), " sdfjlds ")
      runParser ok "" `shouldBe` Just ((), "")
    it "satisfy" $ do
      runParser
        (satisfy isDigit)
        "3"
        `shouldBe` Just ('3', "")
      runParser
        (satisfy odd)
        [3, 4, 1::Int]
        `shouldBe` Just (3, [4, 1])
      runParser
        (satisfy odd)
        [2, 4, 1::Int]
        `shouldBe` Nothing
      runParser
        (satisfy (== 2))
        [2, 4, 1::Int]
        `shouldBe` Just (2, [4, 1])
      runParser
        (satisfy (== 2))
        [1, 4, 1::Int]
        `shouldBe` Nothing
    it "stream" $ do
      runParser
        (stream "hel")
        "hello hi"
        `shouldBe` Just ("hel", "lo hi")
      runParser
        (stream [1::Int, 3, 5])
        [1, 3, 3, 4]
        `shouldBe` Nothing
      runParser
        (stream ["hi", "1"])
        ["hi", "1", "2", "3"]
        `shouldBe` Just (["hi", "1"], ["2", "3"])
