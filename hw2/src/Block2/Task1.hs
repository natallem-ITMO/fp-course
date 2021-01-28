module Block2.Task1 
  (  
     ArithmeticError (..)
  ,  Expr (..)
  ,  eval
  )  where

-- | Representation of expression, that can be a binary expression,
--  has two arguments and perform operation (+) (-) (*) (/) (in integer only) (^) 
-- or can be a constant of Int type
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)


-- | Representation of errors, that can occur while calculating expression.
--   Consists of two error type - division by zero and negative exponentiation
data ArithmeticError
  = DivisionByZero
  | NegativeExponentiation
  deriving (Show, Eq)

-- | Evaluate expression and return either ArithmeticError if any operation
--  while evaluating occurred or result of successful evaluated expression
--  in Int. Consists of many cases to process two kinds of errors
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (Add (Const a) (Const b)) = Right $ a + b
eval (Add a b) = eval a >>= \x -> eval b >>= \y -> eval $ Add (Const x) (Const y)

eval (Sub (Const a) (Const b)) = Right $ a - b
eval (Sub a b) = eval a >>= \x -> eval b >>= \y -> eval $ Sub (Const x) (Const y)

eval (Mul (Const a) (Const b)) = Right $ a * b
eval (Mul a b) = eval a >>= \x -> eval b >>= \y -> eval $ Mul (Const x) (Const y)

eval (Div (Const _) (Const 0)) = Left DivisionByZero
eval (Div (Const x) (Const y)) = Right $ x `div` y
eval (Div a b) = eval a >>= \x -> eval b >>= \y -> eval $ Div (Const x) (Const y)

eval (Pow (Const y) (Const x))
  | x < 0 = Left NegativeExponentiation
  | otherwise = Right $ y ^ x
eval (Pow a b) = eval a >>= \x -> eval b >>= \y -> eval $ Pow (Const x) (Const y)
