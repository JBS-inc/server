{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Int
import           Data.Text.Lazy             (pack)
import           Database.PostgreSQL.Simple
import           Opaleye
import           QRCode
import           Types
import           Web.Scotty
import Data.UUID.V4 (nextRandom)
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
      (liftIO . print) =<< body
      libId   <- param "libid"
      newAchs <- jsonData

      n <- liftIO $ do
        print newAchs
        achs <- mapM (buildAchievement libId) newAchs
        addAchievements conn achs

      liftIO $ print n

      text . pack . show $ n

    post "/achievements/get/" $ do
      libId   <- param "libid"
      expired <- param "expired"
      achs    <- liftIO $ getAchievements conn libId expired
      json achs

getAchievements :: Connection -> Int -> Bool -> IO [Achievement Int]
getAchievements conn libId expired = runQuery conn $
  proc () -> do
    row <- achievementQuery -< ()
    restrict -< a_library_id row .=== constant libId
    returnA -< row

addAchievements :: Connection -> [Achievement (Maybe Int)] -> IO Int64
addAchievements conn achs = runInsertMany conn achievementTable
                          $ fmap constant achs

buildAchievement :: Int -> NewAchievement -> IO (Achievement (Maybe Int))
buildAchievement libId new = do
  uuid <- nextRandom
  return $
    Achievement
      { a_id            = Nothing
      , a_uuid          = uuid
      , a_name          = na_name new
      , a_description   = na_description new
      , a_points        = na_points new
      , a_library_id    = libId
      , a_creation_time = na_creation_time new
      , a_expiry_time   = na_expiry_time new
      , a_hidden        = na_hidden new
      }
