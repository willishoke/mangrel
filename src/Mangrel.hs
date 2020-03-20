-- MANGREL
-- IMAGE SEGMENTATION / RECOLORING
-- WILLIS HOKE


-- Imports

import Codec.Picture
import System.Random
import Data.List
import Data.Ord
import Data.Char
import System.Environment   
import System.Console.ANSI
import Data.Colour.SRGB (sRGB24)
import qualified Data.Vector.Storable as V
import qualified Data.List.Split as LS


-- Type synonyms

type Filename = String
type Picture = ([Pix], Int, Int) 
type PossibleImage = Either String DynamicImage
type Pix = [Float]
type Centroid = [Float]
type Cluster = [Pix]


-- Main

main :: IO ()
main = do  
  let usage = putStrLn "Error: Incorrectly formatted command line arguments."
  putStrLn "" >> putStrLn "𝑀 𝐴 𝑁 𝐺 𝑅 𝐸 𝐿" >> putStrLn ""
  args <- getArgs
  if (length args < 2) || (not $ all isDigit $ args !! 0)
    then usage
    else parseArgs args


-- Parse command line arguments, invoke proper function

parseArgs :: [String] -> IO ()
parseArgs = \args -> do
  let k = read $ head args :: Int
      s1 = "./img/" ++ args !! 1
  if length args < 3
    then go k s1
    else do
      let s2 = "./img/" ++ args !! 2
      go2 k s1 s2 


-- Cluster a single image

go :: Int -> String -> IO ()
go = \k path -> do
  i <- readImage path
  case i of
    Left str -> putStrLn str
    Right img -> do
      let (pic, w, h) = convert img
      p <- palette img k
      putStrLn "Clustering...\n"
      putStr "Palette: " >> printPalette p
      let newPic = recolor pic p
      save (newPic, w, h) "./img/out.png"
      putStrLn "Saved to img/out.png"


-- Take number of clusters, 2 source filepaths
-- Segment first image, replace palette with palette
-- from segmenting second image

go2 :: Int -> String -> String -> IO ()
go2 = \k source1 source2 -> do
  i1 <- readImage source1
  case i1 of
    Left str -> putStrLn str
    Right img -> do
      let (pic, w, h) = convert img
      i2 <- readImage source2
      case i2 of
        Left str2 -> putStrLn str2
        Right img2 -> do
          p1 <- palette img k
          p2 <- palette img2 k
          putStrLn "Recoloring...\n"
          putStr "Palette 1: " >> printPalette p1
          putStr "Palette 2: " >> printPalette p2
          let newPic = recolor2 pic $ zip p1 p2
          save (newPic, w, h) "./img/out.png"
          putStrLn "Saved to img/out.png"


-- Sort a list of centroids from lightest to darkest value

sortKs :: [Centroid] -> [Centroid]
sortKs = sortBy $ comparing $ l2 $ repeat 1.0


-- Take an image and extract palette using clustering

palette :: DynamicImage -> Int -> IO [Centroid]
palette = \i k -> do
  let xs = extractData i
      w = imageWidth $ convertRGB8 i
      h = imageHeight $ convertRGB8 i
  centroids <- initCentroids xs k
  pure $ sortKs $ cluster xs centroids 


-- Print color blocks representing each color in a palette

printPalette :: [Centroid] -> IO ()
printPalette = \ks -> do
  let vals = map (\p -> map (round . (*255)) p) ks
      f = \(r:g:b:_) -> do 
        setSGR [SetRGBColor Foreground $ sRGB24 r g b]
        putStr "██"
  mapM_ f vals
  setSGR [Reset]
  putStrLn "\n"


-- Take a single image and palette, recolor the image

recolor :: [Pix] -> [Centroid] -> [Pix]
recolor = \ps cs ->
  map (closest cs) ps


-- Take an image and pair of palettes
-- Recolor the first image using the second palette

recolor2 :: [Pix] -> [(Centroid, Centroid)] -> [Pix]
recolor2 = \ps cs ->
  let comparing' = \p x y -> compare (p $ fst x) (p $ fst y)
      f = \cs p -> snd $ minimumBy (comparing' $ l2 p) cs
  in map (f cs) ps


-- Randomly select pixels as starting centroid values
-- TODO: Probably doesn't need to be in IO monad

initCentroids :: [Pix] -> Int -> IO [Centroid]
initCentroids = \xs i -> do
  g <- getStdGen
  let rs = randomRs (0, length xs) g :: [Int]
  pure $ map (xs !!) $ take i $ nub rs


-- Normalize image data to range [0, 1]
-- Return a list of all RGB pixel values

extractData :: DynamicImage -> [Pix]
extractData = \i ->
  let img = imageData $ convertRGB8 i
      floats = V.map ((/255) . fromIntegral) img
  in LS.chunksOf 3 $ V.toList floats


-- Get data, width, and height from dynamic image

convert :: DynamicImage -> Picture
convert = \i ->
  let xs = extractData i
      w = imageWidth $ convertRGB8 i
      h = imageHeight $ convertRGB8 i
  in (xs, w, h)


-- Take a 3-tuple of image data, width, and height
-- Data is converted from normalized form to 8-bit values

packData :: Picture -> Image PixelRGB8
packData = \(ps, w, h) ->
  let flat = map (fromIntegral . round . (*255)) $ concat ps
  in Image w h (V.fromList flat)


-- Write a PNG file to the provided file path

save :: Picture -> Filename -> IO ()
save = \pic f ->
  writePng f $ packData pic


-- Assign each pixel to the closest centroid
-- Recalculate value of centroids based on assignments

cluster :: [Pix] -> [Centroid] -> [Centroid]
cluster = \ps cs ->
  let initial = zip (map (closest cs) ps) ps 
      sortF = comparing fst
      groupF = \x y -> fst x == fst y
      grouped = (groupBy groupF . sortBy sortF) initial
  in map (\group -> mean (map snd group)) grouped


-- L2 norm (Euclidean distance) between two
-- sets of RGB pixel values

l2 :: Pix -> Pix -> Float
l2 = \p1 p2 ->
  sqrt $ sum $ map (**2) $ zipWith (-) p1 p2


-- Find mean of list of pixel values
-- Useful for determining brightness

mean :: [Pix] -> Pix
mean = \ps -> 
  let len = fromIntegral $ length ps
  in map ((/len) . sum) $ transpose ps


-- Identify which centroid a pixel is closest to

closest :: [Centroid] -> Pix -> Centroid
closest = \cs p -> 
  minimumBy (comparing $ l2 p) cs


-- end
