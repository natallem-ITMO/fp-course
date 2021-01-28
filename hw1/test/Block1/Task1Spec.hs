module Block1.Task1Spec
  (
     spec
  )  where

import Block1.Task1 (Day(..), afterDays, daysToParty, isWeekend, nextDay)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "nextDay" $ do
    it "all days" $ do
      nextDay Monday `shouldBe` Tuesday
      nextDay Tuesday `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday `shouldBe` Friday
      nextDay Friday `shouldBe` Saturday
      nextDay Saturday `shouldBe` Sunday
      nextDay Sunday `shouldBe` Monday
  describe "afterDays" $ do
    it "after 0" $ do
      afterDays Monday 0 `shouldBe` Monday
      afterDays Tuesday 0 `shouldBe` Tuesday
      afterDays Wednesday 0 `shouldBe` Wednesday
      afterDays Thursday 0 `shouldBe` Thursday
      afterDays Friday 0 `shouldBe` Friday
      afterDays Sunday 0 `shouldBe` Sunday
      afterDays Sunday 0 `shouldBe` Sunday
    it "for Monday" $ do
      afterDays Monday (-9) `shouldBe` Saturday
      afterDays Monday (-7) `shouldBe` Monday
      afterDays Monday (-3) `shouldBe` Friday
      afterDays Monday (-2) `shouldBe` Saturday
      afterDays Monday (-1) `shouldBe` Sunday
      afterDays Monday 1 `shouldBe` Tuesday
      afterDays Monday 2 `shouldBe` Wednesday
      afterDays Monday 3 `shouldBe` Thursday
      afterDays Monday 4 `shouldBe` Friday
      afterDays Monday 5 `shouldBe` Saturday
      afterDays Monday 6 `shouldBe` Sunday
      afterDays Monday 7 `shouldBe` Monday
      afterDays Monday 8 `shouldBe` Tuesday
      afterDays Monday 9 `shouldBe` Wednesday
      afterDays Monday 8641969 `shouldBe` Monday
      afterDays Monday (-8641969) `shouldBe` Monday
    it "randomly" $ do
      afterDays Wednesday (-2) `shouldBe` Monday
      afterDays Wednesday 1 `shouldBe` Thursday
      afterDays Friday 4 `shouldBe` Tuesday
      afterDays Sunday 6 `shouldBe` Saturday
      afterDays Sunday (-6) `shouldBe` Monday
      afterDays Thursday (-31970) `shouldBe` Wednesday
  describe "isWeekend" $ do
    it "all days" $ do
      isWeekend Monday `shouldBe` False
      isWeekend Tuesday `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday `shouldBe` False
      isWeekend Friday `shouldBe` False
      isWeekend Saturday `shouldBe` True
      isWeekend Sunday `shouldBe` True
  describe "daysToParty" $ do
    it "all days" $ do
      daysToParty Monday `shouldBe` 4
      daysToParty Tuesday `shouldBe` 3
      daysToParty Wednesday `shouldBe` 2
      daysToParty Thursday `shouldBe` 1
      daysToParty Friday `shouldBe` 0
      daysToParty Saturday `shouldBe` 6
      daysToParty Sunday `shouldBe` 5
 


