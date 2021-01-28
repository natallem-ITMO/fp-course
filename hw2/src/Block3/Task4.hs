module Block3.Task4
  ( 
    listlistParser
  ) where
  
import Control.Applicative (many)
import Control.Monad (when)
import Data.Char (isSpace)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, satisfy)
import Block3.Task3 (integerParser)


-- | Alias for Parser of Char stream
type StrParser = Parser Char

-- | Parser for zero of more number of space character
spaces :: StrParser String
spaces = many (satisfy isSpace)

-- | Parser of a list of lists of numbers separated by commas. All
--   numbers are listed separated by commas. At the beginning of each
--  list is a number --- the length of the list. This way, you can
--  understand where each list ends.
--  If all input string does not represents correct list of lists,
-- parsing is failed.
listlistParser :: StrParser [[Int]]
listlistParser = ((:) <$> listParser <*> many commaListParser) <* spaces <* eof

-- | Parsing list of numbers, that goes after comma,
-- surrounded by any nymber of spaces
commaListParser :: StrParser [Int]
commaListParser = do
  _ <- betweenSpace (element ',')
  listParser

-- | Use some parser, skipping previous and subsequent spaces
betweenSpace :: StrParser s -> StrParser s
betweenSpace parser = do
  _ <- spaces
  x <- parser
  _ <- spaces
  return x

-- | Parsing one list, first parse number N - length of
-- numbers in list, further trys to parse all N number of
-- array, otherwise fails
listParser :: StrParser [Int]
listParser = do
  x <- betweenSpace integerParser
  when (x < 0) $ fail "negative length"
  readNNumbers x

-- | parsing exactly N numbers of input stream, separeted by commas
readNNumbers :: Integer -> StrParser [Int]
readNNumbers 0 = return []
readNNumbers n = do
  _ <- betweenSpace (element ',')
  x <- integerParser
  rest <- readNNumbers (n - 1)
  return (fromIntegral x : rest)
