{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           Data.IP
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.LocalTime
import           Data.UUID
import           Opaleye

data Library' a b c d e
  = Library
    { l_id            :: a
    , l_name          :: b
    , ip              :: c
    , points_per_hour :: d
    , location        :: e
    }

type Library =
  Library' Int String IP Int String
type LibraryColumn =
  Library' (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

$(makeAdaptorAndInstance "pLibrary" ''Library')

libraryTable :: Table LibraryColumn LibraryColumn
libraryTable = Table "LIBRARIES" $ pLibrary
  Library
    { l_id            = required "id"
    , l_name          = required "name"
    , ip              = required "ip"
    , points_per_hour = required "points_per_hour"
    , location        = required "location"
    }

libraryQuery :: Query LibraryColumn
libraryQuery = queryTable libraryTable

data Achievement' a b c d e f g h i
  = Achievement
    { a_id          :: a
    , uuid          :: b
    , a_name        :: c
    , description   :: d
    , points        :: e
    , a_library_id  :: f
    , creation_time :: g
    , expiry_time   :: h
    , hidden        :: i
    }

type Achievement =
  Achievement' Int UUID String String Int Int LocalTime LocalTime Bool
type AchievementColumn =
  Achievement' (Column PGInt4) (Column PGUuid) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGTimestamp) (Column PGTimestamp) (Column PGBool)

$(makeAdaptorAndInstance "pAchievement" ''Achievement')

achievementTable :: Table AchievementColumn AchievementColumn
achievementTable = Table "ACHIEVEMENTS" $ pAchievement
  Achievement
    { a_id          = required "id"
    , uuid          = required "achievement_uuid"
    , a_name        = required "name"
    , description   = required "description"
    , points        = required "points"
    , a_library_id  = required "library_id"
    , creation_time = required "creation_time"
    , expiry_time   = required "expiry_time"
    , hidden        = required "hidden"
    }

achievementQuery :: Query AchievementColumn
achievementQuery = queryTable achievementTable

data User' a b c
  = User
    { u_id      :: a
    , client_id :: b
    , token     :: c
    }

type User = User' Int String String
type UserColumn = User' (Column PGInt4) (Column PGText) (Column PGText)

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserColumn UserColumn
userTable = Table "USERS" $ pUser
  User
    { u_id      = required "id"
    , client_id = required "client_id"
    , token     = required "token"
    }

userQuery :: Query UserColumn
userQuery = queryTable userTable

data UserAchievement' a b c d
  = UserAchievement
    { ua_id             :: a
    , ua_user_id        :: b
    , ua_achievement_id :: c
    , ua_timestamp      :: d
    }

type UserAchievement =
  UserAchievement' Int Int Int LocalTime
type UserAchievementColumn =
  UserAchievement' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGTimestamp)

$(makeAdaptorAndInstance "pUserAchievement" ''UserAchievement')

userAchievementTable :: Table UserAchievementColumn UserAchievementColumn
userAchievementTable = Table "USER_ACHIEVEMENT_RELATIONS" $ pUserAchievement
  UserAchievement
    { ua_id             = required "id"
    , ua_user_id        = required "user_id"
    , ua_achievement_id = required "achievement_id"
    , ua_timestamp      = required "achievement_time"
    }

userAchievementQuery :: Query UserAchievementColumn
userAchievementQuery = queryTable userAchievementTable

data UserLibrary' a b c d e
  = UserLibrary
    { ul_id         :: a
    , ul_user_id    :: b
    , ul_library_id :: c
    , ul_timestamp  :: d
    , ul_duration   :: e
    }

type UserLibrary =
  UserLibrary' Int Int Int LocalTime TimeOfDay
type UserLibraryColumn =
  UserLibrary' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGTimestamp) (Column PGTime)

$(makeAdaptorAndInstance "pUserLibrary" ''UserLibrary')

userLibraryTable :: Table UserLibraryColumn UserLibraryColumn
userLibraryTable = Table "USER_LIBRARY_RELATIONS" $ pUserLibrary
  UserLibrary
    { ul_id         = required "id"
    , ul_user_id    = required "user_id"
    , ul_library_id = required "library_id"
    , ul_timestamp  = required "visit_time"
    , ul_duration   = required "duration"
    }

userLibraryQuery :: Query UserLibraryColumn
userLibraryQuery = queryTable userLibraryTable