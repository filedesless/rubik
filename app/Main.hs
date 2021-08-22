module Main where

import System.Random
import Data.List

import Rubik

instance Random Orientation where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

display :: (Orientation, Bool) -> String
display (o, True) = show o
display (o, False) = show o ++ "'"

main :: IO ()
main = do
  g <- newStdGen
  let (n, _) = randomR (15, 20) g
  let moves = take n (zip (randoms g :: [Orientation]) (randoms g :: [Bool]))
  let alg = intercalate "" $ display <$> moves
  putStrLn $ "Here's a random shuffle: " ++ alg
  print $ executeAlg alg starter
  putStrLn $ "A solution would be to repeat the alg " ++ show (countBeforeCycle alg) ++ " times"
