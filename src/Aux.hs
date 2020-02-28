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
