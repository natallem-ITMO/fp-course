module Task3Spec
  ( spec,
  )
where

import Control.Monad (forM, forM_, void)
import System.IO.Unsafe ( unsafePerformIO )
import Task3.ConcurrentHashTable
    ( getCHT, newCHT, putCHT, sizeCHT )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "size" $ do
    it "empty table" $ do
      unsafePerformIO
        ( do
            table <- newCHT
            sizeCHT table
        )
        `shouldBe` 0

    it "not empty" $ do
      unsafePerformIO
        ( do
            table <- newCHT
            putCHT "1" (1 :: Int) table
            putCHT "2" 2 table
            putCHT "3" 3 table
            sizeCHT table
        )
        `shouldBe` 3

    it "add 100" $ do
      let num = head maxNumberElements
      addAndReturnResult num
        `shouldBe` (num, map Just [1 .. num])

    it "add 1000" $ do
      let num = maxNumberElements !! 1
      addAndReturnResult num
        `shouldBe` (num, map Just [1 .. num])

    it "add 10000" $ do
      let num = maxNumberElements !! 2
      addAndReturnResult num
        `shouldBe` (num, map Just [1 .. num])

    it "return Nothing" $ do
      let num = maxNumberElements !! 2
      unsafePerformIO
        ( do
            table <- newCHT
            putCHT "a" 1 table
            putCHT "b" 2 table
            putCHT "c" 3 table
            getCHT "d" table
        )
        `shouldBe` Nothing

maxNumberElements :: [Int]
maxNumberElements = [100, 1000, 10000]

addAndReturnResult :: Int -> (Int, [Maybe Int])
addAndReturnResult num =
  unsafePerformIO
    ( do
        table <- newCHT
        forM_ [1 .. num] (\x -> putCHT (show x) x table)
        values <-
          forM
            [1 .. num]
            (flip getCHT table . show)
        s <- sizeCHT table
        return (s, values)
    )
