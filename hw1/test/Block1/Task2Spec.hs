{-# LANGUAGE LambdaCase #-}

module Block1.Task2Spec
  ( 
     spec
  )  where

import Block1.Task2 (Nat (..), divNat, isEven, modNat, natToInteger)

import Control.Exception (ErrorCall (..), evaluate)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "basics operations" $ do
    it "create" $ do
      Z `shouldBe` Z
      S (S Z) `shouldBe` S (S Z)
      S (S $ S Z) `shouldBe` S (S (S Z))
    it "natToInteger" $ do
      natToInteger Z `shouldBe` 0
      natToInteger (S Z) `shouldBe` 1
      natToInteger (S $ S Z) `shouldBe` 2
      natToInteger (S $ S $ S Z) `shouldBe` 3
      natToInteger (S $ S $ S $ S Z) `shouldBe` 4
      natToInteger (S $ S $ S $ S $ S $ S $ S $ S Z) `shouldBe` 8
      natToInteger (S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S Z)
        `shouldBe` 16
    it "integerToNat" $ do
      evaluate (fromInteger (-45) :: Nat)
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot transform -45 to Nat due to its nagetivity" ->
                              True
                          _ -> False
                      )
      evaluate (fromInteger (-1) :: Nat)
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot transform -1 to Nat due to its nagetivity" ->
                              True
                          _ -> False
                      )
      (fromInteger 0 :: Nat) `shouldBe` Z
      (fromInteger 1 :: Nat) `shouldBe` (S Z)
      (fromInteger 2 :: Nat) `shouldBe` S (S Z)
      (fromInteger 3 :: Nat) `shouldBe` S (S $ S Z)
      (fromInteger 4 :: Nat) `shouldBe` S (S $ S $ S Z)
      (fromInteger 8 :: Nat) `shouldBe` S (S $ S $ S $ S $ S $ S $ S Z)
      (fromInteger 16 :: Nat) `shouldBe` S (S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S $ S Z)
  describe "basics arithmetic operations" $ do
    it "sum" $ do
      (0 :: Nat) + (0 :: Nat) `shouldBe` (0 :: Nat)
      (0 :: Nat) + (1 :: Nat) `shouldBe` (1 :: Nat)
      (1 :: Nat) + (0 :: Nat) `shouldBe` (1 :: Nat)
      (1 :: Nat) + (1 :: Nat) `shouldBe` (2 :: Nat)
      (123 :: Nat) + (456 :: Nat) `shouldBe` (579 :: Nat)
    it "mul" $ do
      (0 :: Nat) * (0 :: Nat) `shouldBe` (0 :: Nat)
      (0 :: Nat) * (1 :: Nat) `shouldBe` (0 :: Nat)
      (0 :: Nat) * (1111 :: Nat) `shouldBe` (0 :: Nat)
      (1 :: Nat) * (0 :: Nat) `shouldBe` (0 :: Nat)
      (1111 :: Nat) * (0 :: Nat) `shouldBe` (0 :: Nat)
      (111 :: Nat) * (1 :: Nat) `shouldBe` (111 :: Nat)
      (1 :: Nat) * (111 :: Nat) `shouldBe` (111 :: Nat)
      (64 :: Nat) * (2 :: Nat) `shouldBe` (128 :: Nat)
      (2 :: Nat) * (64 :: Nat) `shouldBe` (128 :: Nat)
      (123 :: Nat) * (456 :: Nat) `shouldBe` (56088 :: Nat)
    it "sub" $ do
      (0 :: Nat) - (0 :: Nat) `shouldBe` (0 :: Nat)
      (0 :: Nat) - (1 :: Nat) `shouldBe` (0 :: Nat)
      (0 :: Nat) - (1111 :: Nat) `shouldBe` (0 :: Nat)
      (1 :: Nat) - (0 :: Nat) `shouldBe` (1 :: Nat)
      (1111 :: Nat) - (0 :: Nat) `shouldBe` (1111 :: Nat)
      (1111 :: Nat) - (10 :: Nat) `shouldBe` (1101 :: Nat)
      (111 :: Nat) - (1 :: Nat) `shouldBe` (110 :: Nat)
      (1 :: Nat) - (111 :: Nat) `shouldBe` (0 :: Nat)
      (64 :: Nat) - (2 :: Nat) `shouldBe` (62 :: Nat)
      (2 :: Nat) - (64 :: Nat) `shouldBe` (0 :: Nat)
      (123 :: Nat) - (456 :: Nat) `shouldBe` (0 :: Nat)
  describe "basics compare operations" $ do
    it "equal" $ do
      (0 :: Nat) == (0 :: Nat) `shouldBe` True
      (1 :: Nat) == (0 :: Nat) `shouldBe` False
      (0 :: Nat) == (1 :: Nat) `shouldBe` False
      (1 :: Nat) == (1 :: Nat) `shouldBe` True
      (123 :: Nat) == (123 :: Nat) `shouldBe` True
      (123 :: Nat) == (456 :: Nat) `shouldBe` False
    it "greater" $ do
      (0 :: Nat) > (0 :: Nat) `shouldBe` False
      (1 :: Nat) > (0 :: Nat) `shouldBe` True
      (0 :: Nat) > (1 :: Nat) `shouldBe` False
      (1 :: Nat) > (1 :: Nat) `shouldBe` False
      (123 :: Nat) > (123 :: Nat) `shouldBe` False
      (456 :: Nat) > (123 :: Nat) `shouldBe` True
      (123 :: Nat) > (456 :: Nat) `shouldBe` False
    it "greater-equal" $ do
      (0 :: Nat) >= (0 :: Nat) `shouldBe` True
      (1 :: Nat) >= (0 :: Nat) `shouldBe` True
      (0 :: Nat) >= (1 :: Nat) `shouldBe` False
      (1 :: Nat) >= (1 :: Nat) `shouldBe` True
      (123 :: Nat) >= (123 :: Nat) `shouldBe` True
      (456 :: Nat) >= (123 :: Nat) `shouldBe` True
      (123 :: Nat) >= (456 :: Nat) `shouldBe` False
    it "less" $ do
      (0 :: Nat) < (0 :: Nat) `shouldBe` False
      (1 :: Nat) < (0 :: Nat) `shouldBe` False
      (0 :: Nat) < (1 :: Nat) `shouldBe` True
      (1 :: Nat) < (1 :: Nat) `shouldBe` False
      (123 :: Nat) < (123 :: Nat) `shouldBe` False
      (456 :: Nat) < (123 :: Nat) `shouldBe` False
      (123 :: Nat) < (456 :: Nat) `shouldBe` True
    it "less-equal" $ do
      (0 :: Nat) <= (0 :: Nat) `shouldBe` True
      (1 :: Nat) <= (0 :: Nat) `shouldBe` False
      (0 :: Nat) <= (1 :: Nat) `shouldBe` True
      (1 :: Nat) <= (1 :: Nat) `shouldBe` True
      (123 :: Nat) <= (123 :: Nat) `shouldBe` True
      (456 :: Nat) <= (123 :: Nat) `shouldBe` False
      (123 :: Nat) <= (456 :: Nat) `shouldBe` True
  describe "div-mod operations" $ do
    it "div" $ do
      evaluate (divNat (0 :: Nat) (0 :: Nat))
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot divide by Zero" 
                            -> True
                          _ -> False
                      )
      evaluate (divNat (34 :: Nat) (0 :: Nat))
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot divide by Zero" 
                            -> True
                          _ -> False
                      )
      divNat (4 :: Nat) (4 :: Nat) `shouldBe` (1 :: Nat)
      divNat (4 :: Nat) (2 :: Nat) `shouldBe` (2 :: Nat)
      divNat (19 :: Nat) (2 :: Nat) `shouldBe` (9 :: Nat)
      divNat (68 :: Nat) (2 :: Nat) `shouldBe` (34 :: Nat)
      divNat (35 :: Nat) (3 :: Nat) `shouldBe` (11 :: Nat)
      divNat (35 :: Nat) (33 :: Nat) `shouldBe` (1 :: Nat)
      divNat (35 :: Nat) (390 :: Nat) `shouldBe` (0 :: Nat)
    it "mod" $ do
      evaluate (modNat (0 :: Nat) (0 :: Nat))
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot divide by Zero" 
                            -> True
                          _ -> False
                      )
      evaluate (modNat (34 :: Nat) (0 :: Nat))
        `shouldThrow` ( \case
                          ErrorCall
                            "Cannot divide by Zero" 
                            -> True
                          _ -> False
                      )
      modNat (4 :: Nat) (4 :: Nat) `shouldBe` (0 :: Nat)
      modNat (4 :: Nat) (2 :: Nat) `shouldBe` (0 :: Nat)
      modNat (19 :: Nat) (2 :: Nat) `shouldBe` (1 :: Nat)
      modNat (68 :: Nat) (3 :: Nat) `shouldBe` (2 :: Nat)
      modNat (35 :: Nat) (4 :: Nat) `shouldBe` (3 :: Nat)
      modNat (35 :: Nat) (33 :: Nat) `shouldBe` (2 :: Nat)
      modNat (178 :: Nat) (167 :: Nat) `shouldBe` (11 :: Nat)
  describe "isEven operation" $ do
    it "even" $ do
      isEven (0 :: Nat) `shouldBe` True
      isEven (2 :: Nat) `shouldBe` True
      isEven (12 :: Nat) `shouldBe` True
      isEven (4398 :: Nat) `shouldBe` True
    it "not even" $ do
      isEven (1 :: Nat) `shouldBe` False
      isEven (5 :: Nat) `shouldBe` False
      isEven (753 :: Nat) `shouldBe` False
      isEven (43987 :: Nat) `shouldBe` False
