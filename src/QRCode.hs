module QRCode where

import           Codec.Binary.QRCode
import           Codec.Picture
import           Data.Array          (Array, (!))
import           Data.Maybe          (fromJust)

qrCodeToPngFile :: FilePath -> String -> IO ()
qrCodeToPngFile path str = savePngImage path
                         . ImageY8
                         . fromJust
                         $ qrCode str

qrCode :: String -> Maybe (Image Pixel8)
qrCode input = do
  v <- version 1
  str <-
    if length input <= 20
    then Just input
    else Nothing

  mat <- encode v M Alphanumeric str
  let arr = toArray mat
      scale = 10
      im =
        generateImage
          (genPixel arr scale $ width mat)
          (scale * (3 + width mat))
          (scale * (3 + width mat))

  return im


genPixel :: Array (Int, Int) Pixel8 -> Int -> Int -> Int -> Int -> Pixel8
genPixel arr scale w x y
  |  x < scale || x >= scale * (w + 2)
  || y < scale || y >= scale * (w + 2)
  = maxBound

  | otherwise
  = arr ! ((x - scale) `div` scale, (y - scale) `div` scale)
