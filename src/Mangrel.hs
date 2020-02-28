
import Codec.Picture
import qualified Data.Vector.Storable as V
import qualified Data.List.Split as LS
import KM

main :: IO ()
main = do  
  eitherImage <- readImage "./vapor.jpg"
  case eitherImage of
    Left str -> putStrLn str
    Right img -> process img

process :: DynamicImage -> IO ()
process = \i -> do
  let xs = extractData i
  let x = KM.l2 "i"
  print $ last xs

--extractData :: DynamicImage -> [[Pixel]]
extractData = \i ->
  let img = imageData $ convertRGB8 i
      floats = V.toList img
  in LS.chunksOf 3 floats
    
packData :: [[Float]] -> DynamicImage
packData = undefined  
