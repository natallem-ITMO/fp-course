module Block2.Task1Spec
  (
     spec
  )  where

import Block2.Task1 (ArithmeticError (..), Expr (..), eval)

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Property, choose, forAll, property)

spec :: Spec
spec = do
  describe "no error operations" $ do
    modifyMaxSuccess (const 1000) $
      it
        "add"
        $ property addTests
    modifyMaxSuccess (const 1000) $
      it
        "sub"
        $ property subTests
    modifyMaxSuccess (const 1000) $
      it
        "mul"
        $ property mulTests
    modifyMaxSuccess (const 1000) $
      it
        "div"
        $ property divTests
  describe "error operations" $ do
    modifyMaxSuccess (const 1000) $
      it
        "div by zero"
        $ property divByZeroTests
    modifyMaxSuccess (const 1000) $
      it
        "negative pow"
        $ property powNegativeTests
    modifyMaxSuccess (const 1000) $
      it
        "contains error in second random argument"
        $ property containsErrorTestsSecond
    modifyMaxSuccess (const 1000) $
      it
        "contains error in first random argument"
        $ property containsErrorTestsFirst

-- | Property tests for combination of correct 
-- add expression consisting of test expression
addTests :: Property
addTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length allLegalExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, allLegalExpr !! j)
     in eval (Add (fst p) (fst q)) == Right (snd p + snd q)

-- | Property tests for combination of correct 
-- sub consisting of test expression
subTests :: Property
subTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length allLegalExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, allLegalExpr !! j)
     in eval (Sub (fst p) (fst q)) == Right (snd p - snd q)

-- | Property tests for combination of correct 
-- mul expression consisting of test expression
mulTests :: Property
mulTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length allLegalExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, allLegalExpr !! j)
     in eval (Mul (fst p) (fst q)) == Right (snd p * snd q)

-- | Property tests for combination of correct 
-- div expression consisting of test expression
divTests :: Property
divTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length legalNegativeExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, legalNegativeExpr !! j)
     in eval (Sub (fst p) (fst q)) == Right (snd p - snd q)

-- | Property tests for div expression with error,
--  combining from tests examples
divByZeroTests :: Property
divByZeroTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length legalZeroExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, legalZeroExpr !! j)
     in eval (Div (fst p) (fst q)) == Left DivisionByZero

-- | Property tests for pow expression with error, 
-- combining from tests examples
powNegativeTests :: Property
powNegativeTests = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length legalZeroExpr - 1)) $ \j ->
    let (p, q) = (allLegalExpr !! i, legalNegativeExpr !! j)
     in eval (Pow (fst p) (fst q)) == Left NegativeExponentiation
     
-- | Property tests for expressions, 
-- containing error expression in first of their operand
containsErrorTestsSecond :: Property
containsErrorTestsSecond = forAll (choose (1, length allLegalExpr - 1)) $ \i ->
  forAll (choose (1, length illegalExpr - 1)) $ \j ->
    forAll (choose (1, length exprConstructors - 1)) $ \v ->
      let (p, q, c) = (allLegalExpr !! i, illegalExpr !! j, exprConstructors !! v)
       in eval (c (fst p) (fst q)) == Left (snd q)

-- | Property tests for expressions, 
-- containing error expression in second of their operand
containsErrorTestsFirst :: Property
containsErrorTestsFirst = forAll (choose (1, length legalPositiveExpr - 1)) $ \i ->
  forAll (choose (1, length illegalExpr - 1)) $ \j ->
    forAll (choose (1, length exprConstructors - 1)) $ \v ->
      let (p, q, c) = (legalPositiveExpr !! i, illegalExpr !! j, exprConstructors !! v)
       in eval (c (fst q) (fst p)) == Left (snd q)

-- | Constructors of expression
exprConstructors :: [Expr -> Expr -> Expr]
exprConstructors = [Add, Mul, Sub, Div, Pow]

-- | Examples of expressions without error with positive result
legalPositiveExpr :: [(Expr, Int)]
legalPositiveExpr =
  [ (Const 1, 1),
    (Const 123, 123),
    (Add (Const 123) (Const 123), 246),
    (Mul (Const 2) (Const 41), 82),
    (Div (Const 12) (Const 1), 12),
    (Div (Const 12) (Const 2), 6),
    (Pow (Const 0) (Const 0), 1),
    (Pow (Const 3) (Const 0), 1),
    (Pow (Const 3) (Const 1), 3),
    (Pow (Const (-3)) (Const 0), 1),
    (Pow (Const (-3)) (Const 2), 9)
  ]

-- | Examples of expressions without error with negative result
legalNegativeExpr :: [(Expr, Int)]
legalNegativeExpr =
  [ (Add (Const 3) (Const (-43)), -40),
    (Sub (Const 3) (Const 43), -40),
    (Mul (Const (-2)) (Const 41), -82),
    (Div (Const 12) (Const (-2)), -6),
    (Pow (Const (-3)) (Const 3), -27)
  ]

-- | Examples of expressions without error with zero result
legalZeroExpr :: [(Expr, Int)]
legalZeroExpr =
  [ (Const 0, 0),
    (Add (Const 123) (Const (-123)), 0),
    (Mul (Const 456) (Const 0), 0),
    (Div (Const 0) (Const 2), 0)
  ]

-- | Examples of expressions with error
illegalExpr :: [(Expr, ArithmeticError)]
illegalExpr =
  [ (Div (Const 12) (Const 0), DivisionByZero),
    (Div (Const 0) (Const 0), DivisionByZero),
    (Div (Const (-123)) (Const 0), DivisionByZero),
    (Pow (Const (-3)) (Const (-3)), NegativeExponentiation),
    (Pow (Const 0) (Const (-100)), NegativeExponentiation)
  ]

-- | All examples of expressions without error
allLegalExpr :: [(Expr, Int)]
allLegalExpr = legalZeroExpr ++ legalNegativeExpr ++ legalPositiveExpr
