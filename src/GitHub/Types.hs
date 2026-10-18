{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitHub.Types where

import           Data.Aeson
    ( FromJSON (..)
    , Options (..)
    , ToJSON (..)
    , camelTo2
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    )

import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

jsonOptions :: Options
jsonOptions = defaultOptions{fieldLabelModifier = camelTo2 '_'}

data User = User
    { login       :: String
    , followers   :: Maybe Int
    , following   :: Maybe Int
    , publicRepos :: Maybe Int
    , company     :: Maybe String
    }
    deriving (Show, Generic)

instance FromJSON User where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON User where
    toJSON = genericToJSON jsonOptions

data Repo = Repo
    { name            :: String
    , stargazersCount :: Int
    , forksCount      :: Int
    , size            :: Int
    , fork            :: Bool
    , archived        :: Bool
    , visibility      :: String
    , language        :: Maybe String
    , languagesUrl    :: String
    , createdAt       :: UTCTime
    , updatedAt       :: UTCTime
    , pushedAt        :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON Repo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Repo where
    toJSON = genericToJSON jsonOptions

data LangStat = LangStat
    { langName  :: String
    , langBytes :: Int
    , langPerc  :: Float
    } deriving (Show, Eq, Generic)

instance FromJSON LangStat where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON LangStat where
    toJSON = genericToJSON jsonOptions

data Org = Org
    { login       :: String
    , description :: Maybe String
    } deriving (Show, Generic)

instance FromJSON Org where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Org where
    toJSON = genericToJSON jsonOptions
