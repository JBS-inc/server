{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class     (liftIO)
import           QRCode
import           Web.Scotty
-- import           Web.Scotty.TLS         (scottyTLS)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Database.PostgreSQL.Simple
import           Opaleye
import           Types

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=futlib"

  -- scottyTLS 3000 "ssl/key.pem" "ssl/cert.pem" $ do
  scotty 3000 $ do
    post "/echo" $ do
      receivedData <- body
      liftIO $ BLC.putStrLn receivedData
      raw receivedData

    get "/qrcode/:arg" $ do
      receivedData <- param "arg"
      liftIO $ qrCodeToPngFile "temp.png" receivedData
      file "temp.png"

    post "/achievements/add" $ do
      receivedData <- body
      liftIO $ BLC.putStrLn receivedData
      raw receivedData

    get "/achievements/get/:arg" $ do
      (ach :: [Achievement]) <- liftIO $ runQuery conn achievementQuery
      json ach

