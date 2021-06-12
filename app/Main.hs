module Main where

import AntAlgorithm
  ( AntAlgorithmField (AntAlgorithmField, bestPath, bestResult),
    Matrix,
    getVisibilityMatrix,
    iterateAntAlgorithm, showMatrix
  )
import Data.List (drop, elem, findIndex, length, map, take)
import Data.Maybe (Maybe (Just, Nothing), fromJust)
import GreedyAlgorithm (getPathByGreedyAlgorithm)
import Random (randomList)
import System.IO
  ( IO,
    IOMode (ReadMode),
    hGetContents,
    openFile,
    print,
  )
import Prelude

generateDistanceMatrix :: Int -> IO Matrix
generateDistanceMatrix num = generateDistanceMatrix' num num

generateDistanceMatrix' :: Int -> Int -> IO Matrix
generateDistanceMatrix' _ 0 = return []
generateDistanceMatrix' num n = do
  list <- randomList num (0, 100)
  rs <- generateDistanceMatrix' num (n -1)
  return (list : rs)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn arr delimiters =
  case findIndex (`elem` delimiters) arr of
    Just num -> take num arr : splitOn (drop (num + 1) arr) delimiters
    Nothing -> [arr]

getMatrix :: String -> IO Matrix
getMatrix path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let lines = map (\a -> map (\a -> read a :: Float) (splitOn a " ")) (splitOn contents "\n")
  return lines

-- hClose handle
-- let matrix = splitOn Int ([a])
-- return contents

-- printMatrix :: IO()
-- printMatrix = do
--   let matrix = randomList 4
--   let str = intercalate "," (map (\a -> show =<< a) matrix)
--   print str

initField :: Matrix -> [[Float]] -> Int -> IO AntAlgorithmField
initField matrix startFeromonMatrix numberOfAnts = do
  (startBestPath, startBestResult) <- getPathByGreedyAlgorithm matrix
  let alpha = 2
  let beta = 3
  let p = 0.2
  let lmin = 34
  firstNodes <- randomList numberOfAnts (0, toInteger (length matrix - 1))
  let antField =
        AntAlgorithmField
          matrix
          (getVisibilityMatrix matrix)
          startFeromonMatrix
          (map (\a -> [floor a] :: [Int]) firstNodes)
          alpha
          beta
          p
          lmin
          (Just startBestPath)
          (Just startBestResult)
  return antField

main :: IO ()
main = do
  matrix <- getMatrix "files/matrix.txt"
  startFeromonMatrix <- getMatrix "files/feromon-matrix.txt"
  print "Start matrix"
  showMatrix matrix
  antField <- initField matrix startFeromonMatrix 3
  print "First found path"
  print (map (+ 1) (fromJust $ bestPath antField))
  print $ bestResult antField

  let newAntFields = iterate iterateAntAlgorithm antField 
  let newAntField = newAntFields !! 10000
  print (map (+ 1) (fromJust $ bestPath newAntField))
  print (bestResult newAntField)

--   print visibilityMatrix