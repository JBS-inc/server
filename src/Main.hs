{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Database.PostgreSQL.Simple
import           Opaleye
import           QRCode
import           Types
import           Web.Scotty
-- import           Web.Scotty.TLS         (scottyTLS)

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

    post "/achievements/get/" $ do
      libId   <- param "libid"
      expired <- param "expired"
      achs    <- liftIO $ getAchievements conn libId expired
      json achs

getAchievements :: Connection -> Int -> Bool -> IO [Achievement]
getAchievements conn libId expired = runQuery conn q
  where
    q = proc () -> do
      row <- achievementQuery -< ()
      restrict -< a_library_id row .=== constant libId
      returnA -< row
