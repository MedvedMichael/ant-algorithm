module AntAlgorithm where

import Control.Monad (replicateM, unless)
import Data.List
  ( drop,
    elemIndex,
    filter,
    foldl,
    head,
    intercalate,
    last,
    length,
    map,
    notElem,
    null,
    tail,
    take,
    zip,
    (!!),
    (++),
  )
import Data.Maybe (Maybe (..), isNothing)
import Random ( randomList )
import System.IO (IO, print)
import System.Random (Random (random, randomR), getStdGen, getStdRandom, randomRIO)
import Prelude

type Matrix = [[Float]]

showMatrix :: Matrix -> IO ()
showMatrix matrix =
  unless (null matrix) $ print (head matrix) >> showMatrix (tail matrix)

replace :: [a] -> Int -> a -> [a]
replace arr num target = take (num -1) arr ++ [target] ++ drop num arr

generateDistanceMatrix :: Int -> IO Matrix
generateDistanceMatrix num = generateDistanceMatrixRecurs num num

generateDistanceMatrixRecurs :: Int -> Int -> IO Matrix
generateDistanceMatrixRecurs _ 0 = return []
generateDistanceMatrixRecurs num n = do
  list <- randomList num (0, 100)
  rs <- generateDistanceMatrixRecurs num (n -1)
  return (replace list (num - n + 1) (-1) : rs)

generateFeromonMatrix :: Int -> IO Matrix
generateFeromonMatrix num = generateFeromonMatrixRecurs num num

generateFeromonMatrixRecurs :: Int -> Int -> IO Matrix
generateFeromonMatrixRecurs _ 0 = return []
generateFeromonMatrixRecurs num n = do
  list <- replicateM num $ randomRIO (0, 1.0)
  rs <- generateFeromonMatrixRecurs num (n -1)
  return (replace list (num - n + 1) 0 : rs)

data AntAlgorithmField = AntAlgorithmField
  { distanceMatrix :: Matrix,
    visibilityMatrix :: Matrix,
    feromonMatrix :: Matrix,
    paths :: [[Int]],
    alpha :: Int,
    beta :: Int,
    p :: Float,
    lmin :: Float,
    bestPath :: Maybe [Int],
    bestResult :: Maybe Float
  }

allPossiblePaths :: Matrix -> [[Int]]
allPossiblePaths matrix = map ([length matrix -1] ++) (allPossiblePathsRecurs (length matrix - 1) [])

allPossiblePathsRecurs :: Int -> [Int] -> [[Int]]
allPossiblePathsRecurs num path =
  if length path == num
    then [path]
    else foldl (\res a -> res ++ allPossiblePathsRecurs num (path ++ [a])) [] (filter (`notElem` path) [0 .. (num -1)])

getVisibilityMatrix :: Matrix -> Matrix
getVisibilityMatrix = map (map (\a -> if a == -1 then 0 else 1 / a))

getPosibilities :: AntAlgorithmField -> [Int] -> [(Int, Float)]
getPosibilities
  (AntAlgorithmField _ visibilityMatrix feromonMatrix _ alpha beta _ _ _ _)
  path =
    map
      ( \a ->
          ( a,
            ( (feromonMatrix !! last path !! a) ^ alpha
                * (visibilityMatrix !! last path !! a) ^ beta
            )
              / sum
          )
      )
      availableNodes
    where
      availableNodes = filter (`notElem` path) [0 .. length visibilityMatrix - 1]
      sum =
        foldl
          ( \result a ->
              result + (feromonMatrix !! last path !! a) ^ alpha
                * (visibilityMatrix !! last path !! a) ^ alpha
          )
          0
          availableNodes

spliceOne :: [a] -> Int -> a -> [a]
spliceOne arr index target = take index arr ++ [target] ++ drop (index + 1) arr

iterateAntAlgorithm :: AntAlgorithmField -> AntAlgorithmField
iterateAntAlgorithm field = iterateAntAlgorithmRecurs field 0

iterateAntAlgorithmRecurs :: AntAlgorithmField -> Int -> AntAlgorithmField
iterateAntAlgorithmRecurs field pathIndex =
  let path = paths field !! pathIndex
   in if length path == length (visibilityMatrix field)
        then
          if pathIndex == length (paths field) - 1
            then recalculateFeromonMatrix field
            else iterateAntAlgorithmRecurs field (pathIndex + 1)
        else
          iterateAntAlgorithmRecurs
            field
              { paths =
                  spliceOne
                    (paths field)
                    pathIndex
                    ( path
                        ++ [ fst $
                               foldl
                                 (\max a -> if snd a > snd max || isNaN (snd a) then a else max)
                                 (-2, 0)
                                 (getPosibilities field path)
                           ]
                    )
              }
            pathIndex

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

includesInPath :: [Int] -> Int -> Int -> Bool
includesInPath path a b =
  let maybeIndexA = elemIndex a path
      maybeIndexB = elemIndex b path
   in not (isNothing maybeIndexA || isNothing maybeIndexB)
        && ( let (Just indexA) = maybeIndexA
                 (Just indexB) = maybeIndexB
              in indexB - indexA == 1
           )

calculateAllPathLength :: AntAlgorithmField -> Int -> Float
calculateAllPathLength (AntAlgorithmField distanceMatrix _ _ paths _ _ _ _ _ _) pathIndex =
  let path = paths !! pathIndex
   in calculatePathLength distanceMatrix path

calculatePathLength :: Matrix -> [Int] -> Float
calculatePathLength distanceMatrix path = calculatePathLengthRecurs path (head path) distanceMatrix 0

calculatePathLengthRecurs :: [Int] -> Int -> Matrix -> Float -> Float
calculatePathLengthRecurs path startIndex matrix result =
  if length path == 1
    then result + matrix !! head path !! startIndex
    else
      let newResult = result + (matrix !! head path !! (path !! 1))
       in calculatePathLengthRecurs (drop 1 path) startIndex matrix newResult

recalculateFeromonMatrix :: AntAlgorithmField -> AntAlgorithmField
recalculateFeromonMatrix field =
  let newResults = map (\a -> (calculateAllPathLength field a, paths field !! a)) [0 .. length (paths field) -1]
      bestNewResult =
        foldl
          ( \res a ->
              if fst res == -1 || fst res > fst a
                then a
                else res
          )
          (-1, [])
          newResults
      (newResult, newPath) = bestNewResult
      deltaFeromon = lmin field / newResult
      maybeLastBestResult = bestResult field
   in field
        { feromonMatrix =
            map
              ( \row ->
                  map
                    ( \a ->
                        let delta = if includesInPath newPath (fst row) (fst a) then deltaFeromon else 0
                         in (1 - p field) * (feromonMatrix field !! fst row !! fst a) + delta
                    )
                    (indexed $ snd row)
              )
              (indexed $ feromonMatrix field),
          bestPath = case bestPath field of
            Nothing -> Just newPath
            Just lastBestPath ->
              case maybeLastBestResult of
                Nothing -> Just newPath
                Just lastBestResult ->
                  if lastBestResult > newResult
                    then Just newPath
                    else Just lastBestPath,
          bestResult =
            case maybeLastBestResult of
              Nothing -> Just newResult
              Just lastBestResult ->
                if lastBestResult > newResult
                  then Just newResult
                  else Just lastBestResult
        }

-- AntAlgorithmField distanceMatrix feromonMatrix numberOfIter path

instance Show AntAlgorithmField where
  show field = intercalate "\n" (map show (feromonMatrix field))