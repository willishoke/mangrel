-- Willis Hoke
-- CS 546
-- Prog 1

module Kmeans where

-- imports 🍷

import System.Environment
import System.Directory
import System.Random
import System.IO
import Control.Monad
import Data.List
import Data.Ord


-- TYPEDEFS  📝

type Point = (Float, Float)
type K = (Int, Point)
type Cluster = (K, [Point])
type Distance = Float
type Range = Point
type Mu = [[Double]]
type Cov = [[Double]]

-- IMPURE FUNCTIONS 😈

main :: IO ()
main = do  
  let k = 3
  args <- getArgs 
  let fileName = head args
  goodFile <- doesFileExist fileName
  if goodFile
    then do
      dataSet <- parseFile fileName 
      print dataSet
      ks <- initKs dataSet k
      print ks
      let classes = classify ks dataSet
      let clusters = center classes
      mapM_ print clusters
    else putStr "Could not find " >> putStrLn fileName

-- turn a file into some data
parseFile :: String -> IO [Point]
parseFile = \fileName -> do
  handle <- openFile fileName ReadMode
  hSetNewlineMode handle universalNewlineMode
  contents <- hGetContents handle 
  let pairs = map words $ lines contents
  let makeTuples = \p -> (read $ p !! 0, read $ p !! 1)
  return $ map makeTuples pairs

-- get random ks within x, y ranges
initKs :: [Point] -> Int -> IO [K]
initKs = \ps k -> do
  let xs = map fst ps
  let ys = map snd ps
  let x = (minimum xs, maximum xs)
  let y = (minimum ys, maximum ys)
  g1 <- newStdGen
  g2 <- newStdGen
  let randxs = take k $ randomRs x g1 :: [Float]
  let randys = take k $ randomRs y g2 :: [Float]
  return $ zip [1..] $ zip randxs randys

initGs :: [Point] -> Int -> IO ([Mu], [Cov])
initGs = undefined

-- PURE FUNCTIONS 😇 

--newKs :: [Classification] -> [Classification]
--newKs = \cs ->
  --undefined

center :: [Cluster] -> [Cluster]
center = \cs -> 
  let newKs = zip (map (fst . fst) cs) $ map (mean . snd) cs
  in zip newKs $ map snd cs
  
 
-- type Cluster = (K, [Point])
classify :: [K] -> [Point] -> [Cluster]
classify = \ks ps -> 
  let initial = zip (map (closest ks) ps) ps
      sortF = comparing $ fst . fst
      groupF = \ab cd -> (fst $ fst ab) == (fst $ fst cd)
      grouped = (groupBy groupF . sortBy sortF) initial
      outer = \list -> (fst $ head list, map snd list)
  in map outer grouped

-- find closest k-value
closest :: [K] -> Point -> K
closest = \ks p -> 
  let dists = map (euclidNorm p) $ map snd ks
      couples = zip ks dists
  in fst $ minimumBy (comparing snd) couples

-- find distance from mean
euclidNorm :: Point -> Point -> Distance
euclidNorm = \point mean -> 
  let x = (fst point) - (fst mean)
      y = (snd point) - (snd mean)
  in sqrt $ x ** 2 + y ** 2

-- find the mean of a set of points
mean :: [Point] -> Point
mean = \ps -> 
  let len = fromIntegral $ length ps
      xsum = (sum $ map fst ps) / len
      ysum = (sum $ map snd ps) / len
  in (xsum, ysum)


-- end
