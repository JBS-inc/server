{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Int
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Text.Lazy             (pack)
import           Data.UUID                  (UUID, fromString, nil)
import           Data.UUID.V4               (nextRandom)
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

    post "/achievements/reset" $ do
      (Just uuid) <- fromString <$> param "uuid"

      n <- liftIO $ do
        newUuid <- nextRandom
        runUpdate conn
                  achievementTableRead
                  (\a -> a { a_uuid = constant newUuid })
                  (\a -> a_uuid a .=== constant uuid)

      text . pack . show $ n

    post "/achievements/add" $ do
      libId   <- param "libid"
      newAchs <- jsonData

      n <- liftIO $ do
        achs <- mapM (newUUID . buildAchievement libId) newAchs
        addAchievements conn achs

      text . pack . show $ n

    post "/achievements/get" $ do
      libId   <- param "libid"
      expired <- param "expired"
      achs    <- liftIO $ getAchievements conn libId expired
      json achs

    post "/user/achievements" $ do
      userId <- param "userid"
      libId  <- param "libid"
      achs   <- liftIO $ getUserAchievements conn userId libId
      liftIO $ print achs
      json achs      

    post "/user/qr" $ do
      userId      <- param "userid"
      (Just uuid) <- fromString <$> param "uuid"
      n <- liftIO $ userQR conn userId uuid
      text . pack . show $ n

    post "/user/score" $ do
      userId <- param "userid"
      libId  <- param "libid"
      n <- liftIO $ getUserScore conn userId libId
      text . pack . show $ n

    post "/user/login" $ do
      username <- param "name"
      n <- liftIO $ getUserName conn username
      text . pack . show $ n

getAchievements :: Connection -> Int -> Bool -> IO [Achievement Int]
getAchievements conn libId expired = runQuery conn $
  proc () -> do
    row <- achievementQuery -< ()
    restrict -< a_library_id row .=== constant libId
    -- restrict -< a_expiry_time row .|| constant libId
    returnA -< row

addAchievements :: Connection -> [Achievement (Maybe Int)] -> IO Int64
addAchievements conn achs = runInsertMany conn achievementTableWrite
                          $ fmap constant achs

newUUID :: Achievement a -> IO (Achievement a)
newUUID ach = do
  uuid <- nextRandom
  return $ ach { a_uuid = uuid }

buildAchievement :: Int -> NewAchievement -> Achievement (Maybe Int)
buildAchievement libId new =
  Achievement
    { a_id            = Nothing
    , a_uuid          = nil
    , a_name          = na_name new
    , a_description   = na_description new
    , a_points        = na_points new
    , a_library_id    = libId
    , a_creation_time = na_creation_time new
    , a_expiry_time   = na_expiry_time new
    , a_hidden        = na_hidden new
    }

userQR :: Connection -> Int -> UUID -> IO Bool
userQR conn userId uuid = do
  (result :: [Achievement Int]) <-
    runQuery conn $
      proc () -> do
        row <- achievementQuery -< ()
        restrict -< a_uuid row .=== constant uuid
        returnA -< row

  case result of
    [x] -> unlockAchivement (a_id x)
    _   -> return False

  where
    unlockAchivement :: Int -> IO Bool
    unlockAchivement achId = do
      t <- getPOSIXTime

      let
        userAchievement :: UserAchievement (Maybe Int)
        userAchievement =
          UserAchievement
            { ua_id             = Nothing
            , ua_user_id        = userId
            , ua_achievement_id = achId
            , ua_timestamp      = round t
            }

      n <- runInsert conn userAchievementTableWrite $ constant userAchievement

      return $ n == 1

getUserAchievements :: Connection -> Int -> Int -> IO [Achievement Int]
getUserAchievements conn userId libId = fst . unzip <$> result
  where
    result :: IO [(Achievement Int, UserAchievementNullable)]
    result = runQuery conn $ getUserAchievementsQuery userId libId

getUserAchievementsQuery :: Int -> Int
                         -> Opaleye.Query ( AchievementColumn (Column PGInt4)
                                          , UserAchievementColumnNullable )
getUserAchievementsQuery userId libId =
  keepWhen (\(a, ua) ->
    (constant (Just userId) .== ua_user_id ua) .&&
    (constant libId .== a_library_id a))
  <<< leftJoin
    achievementQuery
    (queryTable userAchievementTableNullableRead)
    (\(ach, userAch) -> toNullable (a_id ach) .== ua_achievement_id userAch)

getUserScore :: Connection -> Int -> Int -> IO Int
getUserScore conn userId libId = showScore <$> result
  where
    result :: IO [Int]
    result = runQuery conn
           . aggregate Opaleye.sum
           $ pointsArr <<< getUserAchievementsQuery userId libId

    pointsArr :: QueryArr ( AchievementColumn (Column PGInt4)
                          , UserAchievementColumnNullable )
                          ( Column PGInt4 )
    pointsArr = arr (\(a, _) -> a_points a)

    showScore :: [Int] -> Int
    showScore []    = 0
    showScore (x:_) = x

getUserName :: Connection -> String -> IO Int
getUserName conn username = do
  i <- tryGetId
  case i of
    [x] -> return x
    []  -> do
      _ <- runInsert conn userTableWrite $ constant user
      head <$> tryGetId
    _   -> error "Expected a single user, found several"
  where
    user :: User (Maybe Int)
    user = User Nothing username ""

    tryGetId :: IO [Int]
    tryGetId = runQuery conn $
      proc () -> do
        row <- userQuery -< ()
        restrict -< u_client_id row .=== constant username
        returnA -< u_id row

