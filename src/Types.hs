{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           Data.Aeson.TH
import           Data.Int
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.UUID
import           Data.UUID.Aeson ()
import           Opaleye

data Library' a b c d e
  = Library
    { l_id              :: a
    , l_name            :: b
    , l_ip              :: c
    , l_points_per_hour :: d
    , l_location        :: e
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Library')

type Library =
  Library' Int String String Int String
type LibraryColumn =
  Library' (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

$(makeAdaptorAndInstance "pLibrary" ''Library')

libraryTable :: Table LibraryColumn LibraryColumn
libraryTable = Table "libraries" $ pLibrary
  Library
    { l_id              = required "id"
    , l_name            = required "name"
    , l_ip              = required "ip"
    , l_points_per_hour = required "points_per_hour"
    , l_location        = required "location"
    }

libraryQuery :: Query LibraryColumn
libraryQuery = queryTable libraryTable

data NewAchievement
  = NewAchievement
    { na_name          :: String
    , na_description   :: String
    , na_points        :: Int
    , na_creation_time :: Int64
    , na_expiry_time   :: Int64
    , na_hidden        :: Bool
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''NewAchievement)

data Achievement' a b c d e f g h i
  = Achievement
    { a_id            :: a
    , a_uuid          :: b
    , a_name          :: c
    , a_description   :: d
    , a_points        :: e
    , a_library_id    :: f
    , a_creation_time :: g
    , a_expiry_time   :: h
    , a_hidden        :: i
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Achievement')

type Achievement a =
  Achievement' a UUID String String Int Int Int64 Int64 Bool
type AchievementColumn a =
  Achievement' a (Column PGUuid) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt8) (Column PGInt8) (Column PGBool)

$(makeAdaptorAndInstance "pAchievement" ''Achievement')

achievementTableWrite :: Table (AchievementColumn (Maybe (Column PGInt4))) (AchievementColumn (Column PGInt4))
achievementTableWrite = Table "achievements" $ pAchievement
  Achievement
    { a_id            = optional "id"
    , a_uuid          = required "achievement_uuid"
    , a_name          = required "name"
    , a_description   = required "description"
    , a_points        = required "points"
    , a_library_id    = required "library_id"
    , a_creation_time = required "creation_time"
    , a_expiry_time   = required "expiry_time"
    , a_hidden        = required "hidden"
    }

achievementTableRead :: Table (AchievementColumn (Column PGInt4)) (AchievementColumn (Column PGInt4))
achievementTableRead = Table "achievements" $ pAchievement
  Achievement
    { a_id            = required "id"
    , a_uuid          = required "achievement_uuid"
    , a_name          = required "name"
    , a_description   = required "description"
    , a_points        = required "points"
    , a_library_id    = required "library_id"
    , a_creation_time = required "creation_time"
    , a_expiry_time   = required "expiry_time"
    , a_hidden        = required "hidden"
    }

achievementQuery :: Query (AchievementColumn (Column PGInt4))
achievementQuery = queryTable achievementTableRead

data AchievementGet' a b c d
  = AchievementGet
    { ag_name        :: a
    , ag_description :: b
    , ag_points      :: c
    , ag_solved      :: d
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''AchievementGet')

type AchievementGet =
  AchievementGet' String String Int Bool
type AchievementGetColumn =
  AchievementGet' (Column PGText) (Column PGText) (Column PGInt4) (Column PGBool)

$(makeAdaptorAndInstance "pAchievementGet" ''AchievementGet')

data User' a b c
  = User
    { u_id        :: a
    , u_client_id :: b
    , u_token     :: c
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''User')

type User a = User' a String String
type UserColumn a = User' a (Column PGText) (Column PGText)

$(makeAdaptorAndInstance "pUser" ''User')

userTableWrite :: Table (UserColumn (Maybe (Column PGInt4))) (UserColumn (Column PGInt4))
userTableWrite = Table "users" $ pUser
  User
    { u_id        = optional "id"
    , u_client_id = required "client_id"
    , u_token     = required "token"
    }

userTableRead :: Table (UserColumn (Column PGInt4)) (UserColumn (Column PGInt4))
userTableRead = Table "users" $ pUser
  User
    { u_id        = required "id"
    , u_client_id = required "client_id"
    , u_token     = required "token"
    }

userQuery :: Query (UserColumn (Column PGInt4))
userQuery = queryTable userTableRead

data UserAchievement' a b c d
  = UserAchievement
    { ua_id             :: a
    , ua_user_id        :: b
    , ua_achievement_id :: c
    , ua_timestamp      :: d
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''UserAchievement')

type UserAchievement a =
  UserAchievement' a Int Int Int64
type UserAchievementColumn a =
  UserAchievement' a (Column PGInt4) (Column PGInt4) (Column PGInt8)
type UserAchievementNullable =
  UserAchievement' (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int64)
type UserAchievementColumnNullable =
  UserAchievement' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt8))

$(makeAdaptorAndInstance "pUserAchievement" ''UserAchievement')

userAchievementTableWrite :: Table (UserAchievementColumn (Maybe (Column PGInt4))) (UserAchievementColumn (Column PGInt4))
userAchievementTableWrite = Table "user_achievement_relations" $ pUserAchievement
  UserAchievement
    { ua_id             = optional "id"
    , ua_user_id        = required "user_id"
    , ua_achievement_id = required "achievement_id"
    , ua_timestamp      = required "achievement_time"
    }

userAchievementTableRead :: Table (UserAchievementColumn (Column PGInt4)) (UserAchievementColumn (Column PGInt4))
userAchievementTableRead = Table "user_achievement_relations" $ pUserAchievement
  UserAchievement
    { ua_id             = required "id"
    , ua_user_id        = required "user_id"
    , ua_achievement_id = required "achievement_id"
    , ua_timestamp      = required "achievement_time"
    }

userAchievementTableNullableRead :: Table UserAchievementColumnNullable UserAchievementColumnNullable
userAchievementTableNullableRead = Table "user_achievement_relations" $ pUserAchievement
  UserAchievement
    { ua_id             = required "id"
    , ua_user_id        = required "user_id"
    , ua_achievement_id = required "achievement_id"
    , ua_timestamp      = required "achievement_time"
    }

userAchievementQuery :: Query (UserAchievementColumn (Column PGInt4))
userAchievementQuery = queryTable userAchievementTableRead

data UserLibrary' a b c d e
  = UserLibrary
    { ul_id         :: a
    , ul_user_id    :: b
    , ul_library_id :: c
    , ul_timestamp  :: d
    , ul_duration   :: e
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''UserLibrary')

type UserLibrary =
  UserLibrary' Int Int Int Int64 Int64
type UserLibraryColumn =
  UserLibrary' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt8) (Column PGInt8)

$(makeAdaptorAndInstance "pUserLibrary" ''UserLibrary')

userLibraryTable :: Table UserLibraryColumn UserLibraryColumn
userLibraryTable = Table "user_library_relations" $ pUserLibrary
  UserLibrary
    { ul_id         = required "id"
    , ul_user_id    = required "user_id"
    , ul_library_id = required "library_id"
    , ul_timestamp  = required "visit_time"
    , ul_duration   = required "duration"
    }

userLibraryQuery :: Query UserLibraryColumn
userLibraryQuery = queryTable userLibraryTable
