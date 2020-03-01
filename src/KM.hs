module KM where

import Data.List
import Data.Ord

type Pix = [Float]
type Centroid = [Float]
type Cluster = [Pix]

cluster :: [Pix] -> [Centroid] -> [Centroid]
cluster = \ps cs ->
  let initial = zip (map (closest cs) ps) ps 
      sortF = comparing fst
      groupF = \x y -> fst x == fst y
      grouped = (groupBy groupF . sortBy sortF) initial
  in map (\group -> mean (map snd group)) grouped 

l2 :: Pix -> Pix -> Float
l2 = \p1 p2 ->
  sqrt $ sum $ map (**2) $ zipWith (-) p1 p2

mean :: [Pix] -> Pix
mean = \ps -> 
  let len = fromIntegral $ length ps
  in (map ((/len) . sum) . transpose) ps

closest :: [Centroid] -> Pix -> Centroid
closest = \cs p -> 
  minimumBy (comparing $ l2 p) cs
