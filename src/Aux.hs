-- cd :: Dir -> Dir
-- mkdir :: Dir -> IO ()
-- ls :: Dir -> IO ()
-- load :: FilePath -> MImage
-- store :: MImage -> IO ()
-- find :: FileName -> Maybe FilePath

module Aux where

import Graphics.Gloss

--borrow
purple :: [Word8]
purple = [128, 0, 128, 64]

--borrow
bitmapData :: ByteString
bitmapData = pack $ take (4*400*400) (cycle purple)

--borrow
purp :: Picture
purp = bitmapOfByteString 400 400 (BitmapFormat TopToBottom PxRGBA) bitmapData True

--borrow
disp :: Picture -> IO ()
disp = display (InWindow "Mangrel" (400, 400) (10, 10)) white 

(<=?=>) :: [Centroid] -> [Centroid] -> Bool
infix 4 <=?=> -- approximate equality over lists
(<=?=>) = \xs ys ->
  let xs' = map round (concat xs) :: [Int]
      ys' = map round (concat ys) :: [Int]
  in sum (zipWith (-) xs' ys') == 0
   
process :: DynamicImage -> IO ()
process = \i -> do
  let xs = extractData i
  let w = imageWidth $ convertRGB8 i
  let h = imageHeight $ convertRGB8 i
  centroids <- initCentroids xs 3
  let centroids' = cluster xs centroids 
  let newPic = recolor xs centroids'
  print centroids
  print centroids'
  writePng "./hello.png" $ packData w h newPic
