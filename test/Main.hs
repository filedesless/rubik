import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Rubik

main :: IO ()
main = hspec $ do
  describe "rotate" $ do
    it "RR' cancels itself" $ do
      rotate R anticlockwise (rotate R clockwise starter) `shouldBe` starter
    it "RRRR cancels itself" $ do
      rotateN 4 R clockwise starter `shouldBe` starter
