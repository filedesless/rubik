import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Rubik

sexy_move = "RTR'T'"
jperm = "RTR'F'RTR'T'R'FRRT'R'T'"

instance Arbitrary Orientation where
  arbitrary = elements [ F, B, L, R, T, D ]

main :: IO ()
main = hspec $ do
  describe "rotate" $ do
    it "a move cancels it's inverse (like RR')" $ property $
      \o -> rotate o anticlockwise (rotate o clockwise starter) `shouldBe` starter
    it "4 times a move cancels it (like RRRR)" $ property $
      \o -> rotateN 4 o clockwise starter `shouldBe` starter

  describe "executeAlg" $ do
    it ("6 times sexy moves (" ++ sexy_move ++ ") cancels itself") $ do
      executeAlgN 6 sexy_move starter `shouldBe` starter
    it ("2 times jperm (" ++ jperm ++ ") cancels itself") $ do
      executeAlgN 2 jperm starter `shouldBe` starter
