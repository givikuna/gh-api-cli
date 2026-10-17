{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    { login       :: String,
      followers   :: Int,
      following   :: Int,
      publicRepos :: Int
    }
    deriving (Show, Generic)

instance FromJSON User where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON User where
    toJSON = genericToJSON jsonOptions

data Repo = Repo
    { name            :: String,
      stargazersCount :: Int,
      forksCount      :: Int,
      size            :: Int,
      fork            :: Bool,
      archived        :: Bool,
      visibility      :: String,
      language        :: Maybe String,
      languagesUrl    :: String,
      createdAt       :: UTCTime,
      updatedAt       :: UTCTime,
      pushedAt        :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON Repo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Repo where
    toJSON = genericToJSON jsonOptions

data LangStat = LangStat
    { langName  :: String
    , langCount :: Int
    } deriving (Show, Eq)
