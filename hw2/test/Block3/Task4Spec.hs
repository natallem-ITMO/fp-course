module Block3.Task4Spec
  (
     spec
  )  where

import Block3.Task1 (runParser)
import Block3.Task4 (listlistParser)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "correct input" $ do
    it "extreme cases" $ do
      test "" `shouldBe` failRes
      test " " `shouldBe` failRes
      test "   \n  " `shouldBe` failRes
    it "empty arrays" $ do
      test "0" `shouldBe` sucEmptyRes
      test " 0" `shouldBe` sucEmptyRes
      test " \r\r 0" `shouldBe` sucEmptyRes
      test "0\n " `shouldBe` sucEmptyRes
      test "0        " `shouldBe` sucEmptyRes
      test "00" `shouldBe` sucEmptyRes
      test "000" `shouldBe` sucEmptyRes
      test "0  0" `shouldBe` failRes
      test "0 , 0" `shouldBe` sucRes [[], []]
      test "0,0" `shouldBe` sucRes [[], []]
      test "0 , 0   , 0 " `shouldBe` sucRes [[], [], []]
      test "0 , 0   , 0" `shouldBe` sucRes [[], [], []]
    it "one not empty array" $ do
      test "1 , 2" `shouldBe` sucRes [[2]]
      test "1, +2" `shouldBe` sucRes [[2]]
      test "1 ,-2" `shouldBe` sucRes [[-2]]
      test "+1 , -2" `shouldBe` sucRes [[-2]]
      test "+1 ,-2  " `shouldBe` sucRes [[-2]]
      test "   +1,-2" `shouldBe` sucRes [[-2]]
      test "   +1,   -2  " `shouldBe` sucRes [[-2]]
      test "   +1,   -2, 0" `shouldBe` sucRes [[-2], []]
      test "  0,+1,   -2 ,0" `shouldBe` sucRes [[], [-2], []]
      test "0, +1,   -2 ,0  " `shouldBe` sucRes [[], [-2], []]
      test "0, +3,   -2 , 43, +45, 0  " `shouldBe` sucRes [[], [-2, 43, 45], []]
      test "  0, 6,   -2 , 43, +45,  -2 , +43, 45 , 0  "
        `shouldBe` sucRes [[], [-2, 43, 45, -2, 43, 45], []]
    it "many not empty arrays" $ do
      test "1 , 2,  +3,   -2 , 43, +45, 0 " `shouldBe` sucRes [[2], [-2, 43, 45], []]
      test "6,   -2 , 43, +45,  -2 , +43, 45 , 1 , 2,  +3,   -2 , 43, +45, 0 "
        `shouldBe` sucRes [[-2, 43, 45, -2, 43, 45], [2], [-2, 43, 45], []]
      test "1 , 2,  +1, +4, 1, -89 " `shouldBe` sucRes [[2], [4], [-89]]
  describe "incorrect input" $ do
    it "no commas" $ do
      test "1 0" `shouldBe` failRes
      test "1 , 0 3 , 3 ,3,3" `shouldBe` failRes
      test "1 , 0 , 3 , 3 3,3" `shouldBe` failRes
    it "extra commas" $ do
      test "1 , 0 ," `shouldBe` failRes
      test ", 1 , 0 " `shouldBe` failRes
      test "1 , 0 , 3 , 3 ,3,3     ,  " `shouldBe` failRes
      test " ,  1 , 0 , 3 , 3 ,3,3     ,  " `shouldBe` failRes
      test "     ,1 , 0 , 3 , 3 3,3" `shouldBe` failRes
    it "extra numbers" $ do
      test "1,+4,3" `shouldBe` failRes
      test "0 , 2, 4,3,4,2,34" `shouldBe` failRes
    it "lack of numbers" $ do
      test "4,1" `shouldBe` failRes
      test "0, 4,1,2,3,4, 5,1,2,3" `shouldBe` failRes
      test " 0, 4,1,2,3,4, 5,1,2,3" `shouldBe` failRes
    it "not numbers" $ do
      test "1,4f" `shouldBe` failRes
      test "1,f4" `shouldBe` failRes
      test "1,f4ff" `shouldBe` failRes
      test "f1,4" `shouldBe` failRes
      test "f 1,4" `shouldBe` failRes
      test " f   1,4" `shouldBe` failRes
      test "   1,4, 2  hello, 4 , 3 " `shouldBe` failRes
      test "   1,4, 2  , 4 , jdsflk 3 " `shouldBe` failRes
      test "   1,4sdjflk, 2  , 4 , 3 " `shouldBe` failRes
      test "   1,4, --2  , 4 , 3 " `shouldBe` failRes
      test "   1,4, ++2  , 4 , -3 " `shouldBe` failRes
      test "   1,4, +2  , --4 , -3 " `shouldBe` failRes
      test "   1,4, +2  , -4 , -3 !" `shouldBe` failRes

sucRes :: [[Int]] -> Maybe ([[Int]], String)
sucRes res = Just (res, "")

sucEmptyRes :: Maybe ([[Int]], String)
sucEmptyRes = sucRes [[]]

failRes :: Maybe ([[Int]], String)
failRes = Nothing

test :: String -> Maybe ([[Int]], String)
test = runParser listlistParser
