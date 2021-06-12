module Random where
import System.Random (randomRIO, getStdGen, getStdRandom, Random (randomR, random))
import Control.Monad (replicateM)

getRandomNumber :: (Integer, Integer) -> IO Float
getRandomNumber bounds = do
  num <- randomRIO bounds :: IO Integer
  return (fromInteger num::Float)

randomList :: Int -> (Integer, Integer) -> IO [Float]
randomList n bounds = replicateM n $ getRandomNumber bounds

