module Main where

import Rubik

main :: IO ()
main = do
  print (solved starter)
  -- print (solved (rotateRight starter))
  print (solved (executeAlg "R" starter))
  print (solved (executeAlg "RR" starter))
  print (solved (executeAlg "RRR" starter))
  print (solved (executeAlg "RRRR" starter)) -- back to solved
  print (solved (executeAlg "RTR'T'" starter))
  print (solved (executeAlg (take (6 * 6) (cycle "RTR'T'")) starter)) -- 6 times sexy move
  print (solved (executeAlg (take (2 * 23) (cycle "RTR'F'RTR'T'R'FRRT'R'T'")) starter)) -- 2x jperm
