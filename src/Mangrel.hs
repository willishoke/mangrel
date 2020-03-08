import Codec.Picture
import System.Random
import Data.List
import Data.Ord
import qualified Data.Vector.Storable as V
import qualified Data.List.Split as LS
import KM

type Filename = String
type Picture = ([Pix], Int, Int) 

main :: IO ()
main = do  
  i1 <- readImage "./shaq.jpg"
  case i1 of
    Left str -> putStrLn str
    Right img -> do
      let (pic, w, h) = convert img
      i2 <- readImage "./gene.jpg" 
      case i2 of
        Left str2 -> putStrLn str2
        Right img2 -> do
          p1 <- palette img
          p2 <- palette img2
          let newPic = recolor2 pic $ zip p1 p2
          save (newPic, w, h) "both.png"

convert :: DynamicImage -> Picture
convert = \i ->
  let xs = extractData i
      w = imageWidth $ convertRGB8 i
      h = imageHeight $ convertRGB8 i
  in (xs, w, h)

save :: Picture -> Filename -> IO ()
save = \pic f ->
  writePng f $ packData pic

palette :: DynamicImage -> IO [Centroid]
palette = \i -> do
  let xs = extractData i
  let w = imageWidth $ convertRGB8 i
  let h = imageHeight $ convertRGB8 i
  centroids <- initCentroids xs 10
  let f = sortBy $ comparing $ l2 $ repeat 0.0
  pure $ f $ cluster xs centroids 

recolor :: [Pix] -> [Centroid] -> [Pix]
recolor = \ps cs ->
  map (closest cs) ps

recolor2 :: [Pix] -> [(Centroid, Centroid)] -> [Pix]
recolor2 = \ps cs ->
  let comparing' = \p x y -> compare (p $ fst x) (p $ fst y)
      f = \cs p -> snd $ minimumBy (comparing' $ l2 p) cs
  in map (f cs) ps

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
    
packData :: Picture -> Image PixelRGB8
packData = \(ps, w, h) ->
  let flat = map (fromIntegral . round . (*255)) $ concat ps
  in Image w h (V.fromList flat)

  -- end
