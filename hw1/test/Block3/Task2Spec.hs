module Block3.Task2Spec
  ( 
     spec
  )  where

import Block3.Task2 (NonEmpty (..), ThisOrThat (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Semigroup" $ do
    it "NonEmpty <>" $ do
      (firstNonEmpty <> secondNonEmpty) `shouldBe` (1 :| [2, 3, 4, 5, 6, 7])
      (firstNonEmpty <> thirdNonEmpty) `shouldBe` (1 :| [2, 3, 4, 8, 9])
      (secondNonEmpty <> firstNonEmpty) `shouldBe` (5 :| [6, 7, 1, 2, 3, 4])
      (secondNonEmpty <> thirdNonEmpty) `shouldBe` (5 :| [6, 7, 8, 9])
      (thirdNonEmpty <> firstNonEmpty) `shouldBe` (8 :| [9, 1, 2, 3, 4])
      (thirdNonEmpty <> secondNonEmpty) `shouldBe` (8 :| [9, 5, 6, 7])
    it "NonEmpty associativity" $ do
      ((firstNonEmpty <> secondNonEmpty) <> thirdNonEmpty)
        `shouldBe` resultNonEmpty
      (firstNonEmpty <> (secondNonEmpty <> thirdNonEmpty))
        `shouldBe` resultNonEmpty
    it "ThisOrThat <>" $ do
      (this1 <> this2) `shouldBe` this1
      (this1 <> that1) `shouldBe` both1
      (this1 <> both2) `shouldBe` Both 1 'b'
      (that1 <> this1) `shouldBe` both1
      (that1 <> that2) `shouldBe` that1
      (that1 <> both2) `shouldBe` Both 2 'a'
      (both1 <> this2) `shouldBe` both1
      (both1 <> that2) `shouldBe` both1
      (both1 <> both2) `shouldBe` both1
    it "ThisOrThat associativity" $ do
      ((both1 <> both1) <> both2) `shouldBe` (both1 <> (both1 <> both2))
      ((both1 <> both2) <> both1) `shouldBe` (both1 <> (both2 <> both1))
      ((both2 <> both1) <> both1) `shouldBe` (both2 <> (both1 <> both1))
      ((both2 <> both2) <> both1) `shouldBe` (both2 <> (both2 <> both1))
      ((both2 <> both1) <> both2) `shouldBe` (both2 <> (both1 <> both2))
      ((both1 <> both2) <> both2) `shouldBe` (both1 <> (both2 <> both2))
      ((both1 <> both2) <> that1) `shouldBe` (both1 <> (both2 <> that1))
      ((both1 <> that1) <> both2) `shouldBe` (both1 <> (that1 <> both2))
      ((both2 <> both1) <> that1) `shouldBe` (both2 <> (both1 <> that1))
      ((both2 <> that1) <> both1) `shouldBe` (both2 <> (that1 <> both1))
      ((that1 <> both1) <> both2) `shouldBe` (that1 <> (both1 <> both2))
      ((that1 <> both2) <> both1) `shouldBe` (that1 <> (both2 <> both1))
      ((both1 <> both2) <> that2) `shouldBe` (both1 <> (both2 <> that2))
      ((both1 <> that2) <> both2) `shouldBe` (both1 <> (that2 <> both2))
      ((both2 <> both1) <> that2) `shouldBe` (both2 <> (both1 <> that2))
      ((both2 <> that2) <> both1) `shouldBe` (both2 <> (that2 <> both1))
      ((that2 <> both1) <> both2) `shouldBe` (that2 <> (both1 <> both2))
      ((that2 <> both2) <> both1) `shouldBe` (that2 <> (both2 <> both1))
      ((both1 <> both2) <> this1) `shouldBe` (both1 <> (both2 <> this1))
      ((both1 <> this1) <> both2) `shouldBe` (both1 <> (this1 <> both2))
      ((both2 <> both1) <> this1) `shouldBe` (both2 <> (both1 <> this1))
      ((both2 <> this1) <> both1) `shouldBe` (both2 <> (this1 <> both1))
      ((this1 <> both1) <> both2) `shouldBe` (this1 <> (both1 <> both2))
      ((this1 <> both2) <> both1) `shouldBe` (this1 <> (both2 <> both1))
      ((both1 <> both2) <> this2) `shouldBe` (both1 <> (both2 <> this2))
      ((both1 <> this2) <> both2) `shouldBe` (both1 <> (this2 <> both2))
      ((both2 <> both1) <> this2) `shouldBe` (both2 <> (both1 <> this2))
      ((both2 <> this2) <> both1) `shouldBe` (both2 <> (this2 <> both1))
      ((this2 <> both1) <> both2) `shouldBe` (this2 <> (both1 <> both2))
      ((this2 <> both2) <> both1) `shouldBe` (this2 <> (both2 <> both1))
      ((both1 <> both1) <> that1) `shouldBe` (both1 <> (both1 <> that1))
      ((both1 <> that1) <> both1) `shouldBe` (both1 <> (that1 <> both1))
      ((that1 <> both1) <> both1) `shouldBe` (that1 <> (both1 <> both1))
      ((that1 <> that1) <> both1) `shouldBe` (that1 <> (that1 <> both1))
      ((that1 <> both1) <> that1) `shouldBe` (that1 <> (both1 <> that1))
      ((both1 <> that1) <> that1) `shouldBe` (both1 <> (that1 <> that1))
      ((both1 <> that1) <> that2) `shouldBe` (both1 <> (that1 <> that2))
      ((both1 <> that2) <> that1) `shouldBe` (both1 <> (that2 <> that1))
      ((that1 <> both1) <> that2) `shouldBe` (that1 <> (both1 <> that2))
      ((that1 <> that2) <> both1) `shouldBe` (that1 <> (that2 <> both1))
      ((that2 <> both1) <> that1) `shouldBe` (that2 <> (both1 <> that1))
      ((that2 <> that1) <> both1) `shouldBe` (that2 <> (that1 <> both1))
      ((both1 <> that1) <> this1) `shouldBe` (both1 <> (that1 <> this1))
      ((both1 <> this1) <> that1) `shouldBe` (both1 <> (this1 <> that1))
      ((that1 <> both1) <> this1) `shouldBe` (that1 <> (both1 <> this1))
      ((that1 <> this1) <> both1) `shouldBe` (that1 <> (this1 <> both1))
      ((this1 <> both1) <> that1) `shouldBe` (this1 <> (both1 <> that1))
      ((this1 <> that1) <> both1) `shouldBe` (this1 <> (that1 <> both1))
      ((both1 <> that1) <> this2) `shouldBe` (both1 <> (that1 <> this2))
      ((both1 <> this2) <> that1) `shouldBe` (both1 <> (this2 <> that1))
      ((that1 <> both1) <> this2) `shouldBe` (that1 <> (both1 <> this2))
      ((that1 <> this2) <> both1) `shouldBe` (that1 <> (this2 <> both1))
      ((this2 <> both1) <> that1) `shouldBe` (this2 <> (both1 <> that1))
      ((this2 <> that1) <> both1) `shouldBe` (this2 <> (that1 <> both1))
      ((both1 <> both1) <> that2) `shouldBe` (both1 <> (both1 <> that2))
      ((both1 <> that2) <> both1) `shouldBe` (both1 <> (that2 <> both1))
      ((that2 <> both1) <> both1) `shouldBe` (that2 <> (both1 <> both1))
      ((that2 <> that2) <> both1) `shouldBe` (that2 <> (that2 <> both1))
      ((that2 <> both1) <> that2) `shouldBe` (that2 <> (both1 <> that2))
      ((both1 <> that2) <> that2) `shouldBe` (both1 <> (that2 <> that2))
      ((both1 <> that2) <> this1) `shouldBe` (both1 <> (that2 <> this1))
      ((both1 <> this1) <> that2) `shouldBe` (both1 <> (this1 <> that2))
      ((that2 <> both1) <> this1) `shouldBe` (that2 <> (both1 <> this1))
      ((that2 <> this1) <> both1) `shouldBe` (that2 <> (this1 <> both1))
      ((this1 <> both1) <> that2) `shouldBe` (this1 <> (both1 <> that2))
      ((this1 <> that2) <> both1) `shouldBe` (this1 <> (that2 <> both1))
      ((both1 <> that2) <> this2) `shouldBe` (both1 <> (that2 <> this2))
      ((both1 <> this2) <> that2) `shouldBe` (both1 <> (this2 <> that2))
      ((that2 <> both1) <> this2) `shouldBe` (that2 <> (both1 <> this2))
      ((that2 <> this2) <> both1) `shouldBe` (that2 <> (this2 <> both1))
      ((this2 <> both1) <> that2) `shouldBe` (this2 <> (both1 <> that2))
      ((this2 <> that2) <> both1) `shouldBe` (this2 <> (that2 <> both1))
      ((both1 <> both1) <> this1) `shouldBe` (both1 <> (both1 <> this1))
      ((both1 <> this1) <> both1) `shouldBe` (both1 <> (this1 <> both1))
      ((this1 <> both1) <> both1) `shouldBe` (this1 <> (both1 <> both1))
      ((this1 <> this1) <> both1) `shouldBe` (this1 <> (this1 <> both1))
      ((this1 <> both1) <> this1) `shouldBe` (this1 <> (both1 <> this1))
      ((both1 <> this1) <> this1) `shouldBe` (both1 <> (this1 <> this1))
      ((both1 <> this1) <> this2) `shouldBe` (both1 <> (this1 <> this2))
      ((both1 <> this2) <> this1) `shouldBe` (both1 <> (this2 <> this1))
      ((this1 <> both1) <> this2) `shouldBe` (this1 <> (both1 <> this2))
      ((this1 <> this2) <> both1) `shouldBe` (this1 <> (this2 <> both1))
      ((this2 <> both1) <> this1) `shouldBe` (this2 <> (both1 <> this1))
      ((this2 <> this1) <> both1) `shouldBe` (this2 <> (this1 <> both1))
      ((both1 <> both1) <> this2) `shouldBe` (both1 <> (both1 <> this2))
      ((both1 <> this2) <> both1) `shouldBe` (both1 <> (this2 <> both1))
      ((this2 <> both1) <> both1) `shouldBe` (this2 <> (both1 <> both1))
      ((this2 <> this2) <> both1) `shouldBe` (this2 <> (this2 <> both1))
      ((this2 <> both1) <> this2) `shouldBe` (this2 <> (both1 <> this2))
      ((both1 <> this2) <> this2) `shouldBe` (both1 <> (this2 <> this2))
      ((both2 <> both2) <> that1) `shouldBe` (both2 <> (both2 <> that1))
      ((both2 <> that1) <> both2) `shouldBe` (both2 <> (that1 <> both2))
      ((that1 <> both2) <> both2) `shouldBe` (that1 <> (both2 <> both2))
      ((that1 <> that1) <> both2) `shouldBe` (that1 <> (that1 <> both2))
      ((that1 <> both2) <> that1) `shouldBe` (that1 <> (both2 <> that1))
      ((both2 <> that1) <> that1) `shouldBe` (both2 <> (that1 <> that1))
      ((both2 <> that1) <> that2) `shouldBe` (both2 <> (that1 <> that2))
      ((both2 <> that2) <> that1) `shouldBe` (both2 <> (that2 <> that1))
      ((that1 <> both2) <> that2) `shouldBe` (that1 <> (both2 <> that2))
      ((that1 <> that2) <> both2) `shouldBe` (that1 <> (that2 <> both2))
      ((that2 <> both2) <> that1) `shouldBe` (that2 <> (both2 <> that1))
      ((that2 <> that1) <> both2) `shouldBe` (that2 <> (that1 <> both2))
      ((both2 <> that1) <> this1) `shouldBe` (both2 <> (that1 <> this1))
      ((both2 <> this1) <> that1) `shouldBe` (both2 <> (this1 <> that1))
      ((that1 <> both2) <> this1) `shouldBe` (that1 <> (both2 <> this1))
      ((that1 <> this1) <> both2) `shouldBe` (that1 <> (this1 <> both2))
      ((this1 <> both2) <> that1) `shouldBe` (this1 <> (both2 <> that1))
      ((this1 <> that1) <> both2) `shouldBe` (this1 <> (that1 <> both2))
      ((both2 <> that1) <> this2) `shouldBe` (both2 <> (that1 <> this2))
      ((both2 <> this2) <> that1) `shouldBe` (both2 <> (this2 <> that1))
      ((that1 <> both2) <> this2) `shouldBe` (that1 <> (both2 <> this2))
      ((that1 <> this2) <> both2) `shouldBe` (that1 <> (this2 <> both2))
      ((this2 <> both2) <> that1) `shouldBe` (this2 <> (both2 <> that1))
      ((this2 <> that1) <> both2) `shouldBe` (this2 <> (that1 <> both2))
      ((both2 <> both2) <> that2) `shouldBe` (both2 <> (both2 <> that2))
      ((both2 <> that2) <> both2) `shouldBe` (both2 <> (that2 <> both2))
      ((that2 <> both2) <> both2) `shouldBe` (that2 <> (both2 <> both2))
      ((that2 <> that2) <> both2) `shouldBe` (that2 <> (that2 <> both2))
      ((that2 <> both2) <> that2) `shouldBe` (that2 <> (both2 <> that2))
      ((both2 <> that2) <> that2) `shouldBe` (both2 <> (that2 <> that2))
      ((both2 <> that2) <> this1) `shouldBe` (both2 <> (that2 <> this1))
      ((both2 <> this1) <> that2) `shouldBe` (both2 <> (this1 <> that2))
      ((that2 <> both2) <> this1) `shouldBe` (that2 <> (both2 <> this1))
      ((that2 <> this1) <> both2) `shouldBe` (that2 <> (this1 <> both2))
      ((this1 <> both2) <> that2) `shouldBe` (this1 <> (both2 <> that2))
      ((this1 <> that2) <> both2) `shouldBe` (this1 <> (that2 <> both2))
      ((both2 <> that2) <> this2) `shouldBe` (both2 <> (that2 <> this2))
      ((both2 <> this2) <> that2) `shouldBe` (both2 <> (this2 <> that2))
      ((that2 <> both2) <> this2) `shouldBe` (that2 <> (both2 <> this2))
      ((that2 <> this2) <> both2) `shouldBe` (that2 <> (this2 <> both2))
      ((this2 <> both2) <> that2) `shouldBe` (this2 <> (both2 <> that2))
      ((this2 <> that2) <> both2) `shouldBe` (this2 <> (that2 <> both2))
      ((both2 <> both2) <> this1) `shouldBe` (both2 <> (both2 <> this1))
      ((both2 <> this1) <> both2) `shouldBe` (both2 <> (this1 <> both2))
      ((this1 <> both2) <> both2) `shouldBe` (this1 <> (both2 <> both2))
      ((this1 <> this1) <> both2) `shouldBe` (this1 <> (this1 <> both2))
      ((this1 <> both2) <> this1) `shouldBe` (this1 <> (both2 <> this1))
      ((both2 <> this1) <> this1) `shouldBe` (both2 <> (this1 <> this1))
      ((both2 <> this1) <> this2) `shouldBe` (both2 <> (this1 <> this2))
      ((both2 <> this2) <> this1) `shouldBe` (both2 <> (this2 <> this1))
      ((this1 <> both2) <> this2) `shouldBe` (this1 <> (both2 <> this2))
      ((this1 <> this2) <> both2) `shouldBe` (this1 <> (this2 <> both2))
      ((this2 <> both2) <> this1) `shouldBe` (this2 <> (both2 <> this1))
      ((this2 <> this1) <> both2) `shouldBe` (this2 <> (this1 <> both2))
      ((both2 <> both2) <> this2) `shouldBe` (both2 <> (both2 <> this2))
      ((both2 <> this2) <> both2) `shouldBe` (both2 <> (this2 <> both2))
      ((this2 <> both2) <> both2) `shouldBe` (this2 <> (both2 <> both2))
      ((this2 <> this2) <> both2) `shouldBe` (this2 <> (this2 <> both2))
      ((this2 <> both2) <> this2) `shouldBe` (this2 <> (both2 <> this2))
      ((both2 <> this2) <> this2) `shouldBe` (both2 <> (this2 <> this2))
      ((that1 <> that1) <> that2) `shouldBe` (that1 <> (that1 <> that2))
      ((that1 <> that2) <> that1) `shouldBe` (that1 <> (that2 <> that1))
      ((that2 <> that1) <> that1) `shouldBe` (that2 <> (that1 <> that1))
      ((that2 <> that2) <> that1) `shouldBe` (that2 <> (that2 <> that1))
      ((that2 <> that1) <> that2) `shouldBe` (that2 <> (that1 <> that2))
      ((that1 <> that2) <> that2) `shouldBe` (that1 <> (that2 <> that2))
      ((that1 <> that2) <> this1) `shouldBe` (that1 <> (that2 <> this1))
      ((that1 <> this1) <> that2) `shouldBe` (that1 <> (this1 <> that2))
      ((that2 <> that1) <> this1) `shouldBe` (that2 <> (that1 <> this1))
      ((that2 <> this1) <> that1) `shouldBe` (that2 <> (this1 <> that1))
      ((this1 <> that1) <> that2) `shouldBe` (this1 <> (that1 <> that2))
      ((this1 <> that2) <> that1) `shouldBe` (this1 <> (that2 <> that1))
      ((that1 <> that2) <> this2) `shouldBe` (that1 <> (that2 <> this2))
      ((that1 <> this2) <> that2) `shouldBe` (that1 <> (this2 <> that2))
      ((that2 <> that1) <> this2) `shouldBe` (that2 <> (that1 <> this2))
      ((that2 <> this2) <> that1) `shouldBe` (that2 <> (this2 <> that1))
      ((this2 <> that1) <> that2) `shouldBe` (this2 <> (that1 <> that2))
      ((this2 <> that2) <> that1) `shouldBe` (this2 <> (that2 <> that1))
      ((that1 <> that1) <> this1) `shouldBe` (that1 <> (that1 <> this1))
      ((that1 <> this1) <> that1) `shouldBe` (that1 <> (this1 <> that1))
      ((this1 <> that1) <> that1) `shouldBe` (this1 <> (that1 <> that1))
      ((this1 <> this1) <> that1) `shouldBe` (this1 <> (this1 <> that1))
      ((this1 <> that1) <> this1) `shouldBe` (this1 <> (that1 <> this1))
      ((that1 <> this1) <> this1) `shouldBe` (that1 <> (this1 <> this1))
      ((that1 <> this1) <> this2) `shouldBe` (that1 <> (this1 <> this2))
      ((that1 <> this2) <> this1) `shouldBe` (that1 <> (this2 <> this1))
      ((this1 <> that1) <> this2) `shouldBe` (this1 <> (that1 <> this2))
      ((this1 <> this2) <> that1) `shouldBe` (this1 <> (this2 <> that1))
      ((this2 <> that1) <> this1) `shouldBe` (this2 <> (that1 <> this1))
      ((this2 <> this1) <> that1) `shouldBe` (this2 <> (this1 <> that1))
      ((that1 <> that1) <> this2) `shouldBe` (that1 <> (that1 <> this2))
      ((that1 <> this2) <> that1) `shouldBe` (that1 <> (this2 <> that1))
      ((this2 <> that1) <> that1) `shouldBe` (this2 <> (that1 <> that1))
      ((this2 <> this2) <> that1) `shouldBe` (this2 <> (this2 <> that1))
      ((this2 <> that1) <> this2) `shouldBe` (this2 <> (that1 <> this2))
      ((that1 <> this2) <> this2) `shouldBe` (that1 <> (this2 <> this2))
      ((that2 <> that2) <> this1) `shouldBe` (that2 <> (that2 <> this1))
      ((that2 <> this1) <> that2) `shouldBe` (that2 <> (this1 <> that2))
      ((this1 <> that2) <> that2) `shouldBe` (this1 <> (that2 <> that2))
      ((this1 <> this1) <> that2) `shouldBe` (this1 <> (this1 <> that2))
      ((this1 <> that2) <> this1) `shouldBe` (this1 <> (that2 <> this1))
      ((that2 <> this1) <> this1) `shouldBe` (that2 <> (this1 <> this1))
      ((that2 <> this1) <> this2) `shouldBe` (that2 <> (this1 <> this2))
      ((that2 <> this2) <> this1) `shouldBe` (that2 <> (this2 <> this1))
      ((this1 <> that2) <> this2) `shouldBe` (this1 <> (that2 <> this2))
      ((this1 <> this2) <> that2) `shouldBe` (this1 <> (this2 <> that2))
      ((this2 <> that2) <> this1) `shouldBe` (this2 <> (that2 <> this1))
      ((this2 <> this1) <> that2) `shouldBe` (this2 <> (this1 <> that2))
      ((that2 <> that2) <> this2) `shouldBe` (that2 <> (that2 <> this2))
      ((that2 <> this2) <> that2) `shouldBe` (that2 <> (this2 <> that2))
      ((this2 <> that2) <> that2) `shouldBe` (this2 <> (that2 <> that2))
      ((this2 <> this2) <> that2) `shouldBe` (this2 <> (this2 <> that2))
      ((this2 <> that2) <> this2) `shouldBe` (this2 <> (that2 <> this2))
      ((that2 <> this2) <> this2) `shouldBe` (that2 <> (this2 <> this2))
      ((this1 <> this1) <> this2) `shouldBe` (this1 <> (this1 <> this2))
      ((this1 <> this2) <> this1) `shouldBe` (this1 <> (this2 <> this1))
      ((this2 <> this1) <> this1) `shouldBe` (this2 <> (this1 <> this1))
      ((this2 <> this2) <> this1) `shouldBe` (this2 <> (this2 <> this1))
      ((this2 <> this1) <> this2) `shouldBe` (this2 <> (this1 <> this2))
      ((this1 <> this2) <> this2) `shouldBe` (this1 <> (this2 <> this2))

-- | Bellow NonEmpty and ThisOrThat 
-- functions are just examples for tests

firstNonEmpty :: NonEmpty Int
firstNonEmpty = 1 :| [2, 3, 4]

secondNonEmpty :: NonEmpty Int
secondNonEmpty = 5 :| [6, 7]

thirdNonEmpty :: NonEmpty Int
thirdNonEmpty = 8 :| [9]

resultNonEmpty :: NonEmpty Int
resultNonEmpty = 1 :| [2, 3, 4, 5, 6, 7, 8, 9]

this1 :: ThisOrThat Int Char
this1 = This 1

this2 :: ThisOrThat Int Char
this2 = This 2

that1 :: ThisOrThat Int Char
that1 = That 'a'

that2 :: ThisOrThat Int Char
that2 = That 'a'

both1 :: ThisOrThat Int Char
both1 = Both 1 'a'

both2 :: ThisOrThat Int Char
both2 = Both 2 'b'
