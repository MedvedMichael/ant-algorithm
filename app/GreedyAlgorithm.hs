module GreedyAlgorithm where
import AntAlgorithm (AntAlgorithmField (distanceMatrix), Matrix, calculatePathLength)
import Random ( getRandomNumber )

getPathByGreedyAlgorithm :: Matrix -> IO ([Int], Float)
getPathByGreedyAlgorithm matrix = getPathByGreedyAlgorithmRecurs matrix []

getPathByGreedyAlgorithmRecurs :: Matrix -> [Int] -> IO ([Int], Float)
getPathByGreedyAlgorithmRecurs matrix path =
  let allLength = length matrix
   in if null path
        then do
          num <- getRandomNumber (0, toInteger (length matrix -1))
          getPathByGreedyAlgorithmRecurs matrix [floor num]
        else
          if length path == allLength
            then do
              return (path, calculatePathLength matrix path)
            else do
              let
                row = matrix !! last path
                availableNodes = filter (`notElem` path) [0..length matrix -1]
                minIndex = foldl (\res a -> if row !! a == -1 || row !! res < row !! a then res else a) (head availableNodes) availableNodes
              getPathByGreedyAlgorithmRecurs matrix (path ++ [minIndex])