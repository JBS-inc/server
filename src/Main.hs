{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)
import           QRCode
import           Web.Scotty
import           Web.Scotty.TLS         (scottyTLS)

main :: IO ()
main = do
  scottyTLS 3000 "ssl/key.pem" "ssl/cert.pem" $ do
    get "/:word" $ do
      beam <- param "word"
      liftIO $ qrCodeToPngFile "qrcode.png" beam
      file "qrcode.png"

