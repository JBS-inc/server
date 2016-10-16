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
qrCode str = do
  v <- version 2
  mat <- encode v L Alphanumeric str
  let arr = toArray mat
      scale = 10
      border = 2 * scale
      w = 1 + width mat
      im =
        generateImage
          (genPixel arr scale border w)
          (scale * w + 2 * border)
          (scale * w + 2 * border)

  return im

genPixel :: Array (Int, Int) Pixel8 -> Int -> Int -> Int -> Int -> Int -> Pixel8
genPixel arr scale border w x y
  |  x < border || x >= border + scale * w
  || y < border || y >= border + scale * w
  = maxBound

  | otherwise
  = arr ! ((x - border) `div` scale, (y - border) `div` scale)
