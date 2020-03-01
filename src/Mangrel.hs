import Codec.Picture
import System.Random
import Data.List (nub)
import qualified Data.Vector.Storable as V
import qualified Data.List.Split as LS
import KM

main :: IO ()
main = do  
  eitherImage <- readImage "./mantis.jpg"
  case eitherImage of
    Left str -> putStrLn str
    Right img -> process img

process :: DynamicImage -> IO ()
process = \i -> do
  let xs = extractData i
  let w = imageWidth $ convertRGB8 i
  let h = imageHeight $ convertRGB8 i
  centroids <- initCentroids xs 8
  let centroids' = cluster xs centroids 
  let newPic = recolor xs centroids'
  print centroids
  print centroids'
  writePng "./hello.png" $ packData w h newPic

recolor :: [Pix] -> [Centroid] -> [Pix]
recolor = \ps cs ->
  map (closest cs) ps

initCentroids :: [Pix] -> Int -> IO [Centroid]
initCentroids = \xs i -> do
  g <- getStdGen
  let rs = randomRs (0, length xs) g :: [Int]
  pure $ map (xs !!) $ take i $ nub rs

extractData :: DynamicImage -> [Pix]
extractData = \i ->
  let img = imageData $ convertRGB8 i
      floats = V.map ((/255) . fromIntegral) img
  in LS.chunksOf 3 $ V.toList floats
    
packData :: Int -> Int -> [Pix] -> Image PixelRGB8
packData = \h w ps ->
  let flat = map (fromIntegral . round . (*255)) $ concat ps
  in Image h w (V.fromList flat)

-- end
