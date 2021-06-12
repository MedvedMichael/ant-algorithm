module Random where

import Control.Monad (replicateM)
import System.Random (Random (random, randomR), getStdGen, getStdRandom, randomRIO)

getRandomNumber :: (Integer, Integer) -> IO Float
getRandomNumber bounds = do
  num <- randomRIO bounds :: IO Integer
  return (fromInteger num :: Float)

randomList :: Int -> (Integer, Integer) -> IO [Float]
randomList n bounds = replicateM n $ getRandomNumber bounds
