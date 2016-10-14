{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty
import           Web.Scotty.TLS (scottyTLS)

import           QRCode

main :: IO ()
main = do
  qrCodeToPngFile "surprise.png" "Whats up Jack"

  scottyTLS 3000 "ssl/key.pem" "ssl/cert.pem" $ do
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

