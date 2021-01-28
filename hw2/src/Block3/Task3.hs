module Block3.Task3
  ( 
     CBS
  ,  correctBracketSequence
  ,  integerParser,
  )  where
  
import Control.Applicative ((<|>))
import Data.Char (isDigit)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok)

type StrParser = Parser Char

-- | Representation of data for CBS
--   EmptyCBS - empty CBS
--   InBrackets - (CBS)CBS*
data CBS
  = EmptyCBS ()
  | InBrackets CBS CBS

-- | Instance uses in tests
instance Show CBS where
  show (EmptyCBS ()) = ""
  show (InBrackets x y) = "(" ++ show x ++ ")" ++ show y

-- | Parser check if all given string is CBF, return CBS data type if so,
-- otherwise fails
correctBracketSequence :: StrParser CBS
correctBracketSequence = oneCBS <* eof
  where
    oneCBS = (InBrackets <$> inBrackets <*> oneCBS) <|> (EmptyCBS <$> ok)
    inBrackets = (\_ x _ -> x) <$> element '(' <*> oneCBS <*> element ')'

-- | Parsing integer that can be represented as <+"num"> or <-"num"> or <"num">
--  where "num" is sequence of digits characters. If stream contains such number,
-- return it and rest of stream, otherwise fails
integerParser :: StrParser Integer
integerParser =
  signConstructor
    <$> (element '+' <|> element '-' <|> return 'x')
    <*> simpleIntegerParser
  where
  
    signConstructor :: Char -> Integer -> Integer
    signConstructor '-' num = negate num
    signConstructor _ num = num

    simpleIntegerParser = Parser f
      where
        f xs
          | null ns = Nothing
          | otherwise = Just (read ns, rest)
          where
            (ns, rest) = span isDigit xs
