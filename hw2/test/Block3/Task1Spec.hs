module Block3.Task1Spec
  (
     spec
  )  where

import Control.Applicative (Alternative (..), empty)

import Block3.Task1 (Parser (..), runParser)
import Block3.Task2 (element, eof, ok)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "functor instance" $ do
    it "fmap" $ do
      runParser
        (fmap (\x -> show (x + 3)) (getReturnConstParser (3 :: Int)))
        "hello"
        `shouldBe` Just ("6", "hello")
      runParser
        (fmap ("123" ++) (getReturnConstParser "45"))
        "hello"
        `shouldBe` Just ("12345", "hello")
      runParser
        (fmap snd (getReturnConstParser (3 :: Int, 'd')))
        "hello"
        `shouldBe` Just ('d', "hello")
  describe "applicative instance" $ do
    it "pure" $ do
      runParser
        (pure "h")
        "hello"
        `shouldBe` Just ("h", "hello")
      runParser
        (pure 'h')
        ["hello", "hi"]
        `shouldBe` Just ('h', ["hello", "hi"])
      runParser
        (pure (4 :: Int))
        "haha"
        `shouldBe` Just (4, "haha")
    it "<*>" $ do
      runParser
        (pure (+ 5) <*> pure (4 :: Int))
        "hello"
        `shouldBe` Just (9, "hello")
      runParser
        (pure ("hw" ++) <*> pure "2")
        "hello"
        `shouldBe` Just ("hw2", "hello")
      runParser
        (pure show <*> pure (2::Int))
        "hello"
        `shouldBe` Just ("2", "hello")
  describe "alternative instance" $ do
    it "empty" $ do
      runParser
        (empty :: (Parser Char Int))
        "hello"
        `shouldBe` Nothing
      runParser
        (empty :: (Parser String Char))
        ["h"]
        `shouldBe` Nothing
      runParser
        (empty :: (Parser [String] String))
        [["df"]]
        `shouldBe` Nothing
    it "<|>" $ do
      runParser
        (element 'x' <|> element 'y')
        "xyx"
        `shouldBe` Just ('x', "yx")
      runParser
        (eof <|> ok)
        "xyx"
        `shouldBe` Just ((), "xyx")
      runParser
        (Parser (\(_ : y) -> return ((), 's' : y)) <|> ok)
        "xyx"
        `shouldBe` Just ((), "syx")
      runParser
        (ok <|> Parser (\(_ : y) -> return ((), 's' : y)))
        "xyx"
        `shouldBe` Just ((), "xyx")
  describe "monad instance" $ do
    it ">>=" $ do
      runParser
        (element 's' >> element 'd')
        "sdf"
        `shouldBe` Just ('d', "f")
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
        "sdf"
        `shouldBe` Nothing
      runParser
        ( do
            x <- element 's'
            y <- element 'y'
            _ <- eof
            return [x, y]
        )
        "syf"
        `shouldBe` Nothing
      runParser
        ( do
            x <- element 's'
            y <- element 'y'
            _ <- eof
            return [x, y]
        )
        "sy"
        `shouldBe` Just ("sy", "")

-- | Analog of pure, but cannot use pure because of testing all functionality
getReturnConstParser :: a -> Parser Char a
getReturnConstParser arg = Parser $ \str -> return (arg, str)
