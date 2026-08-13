{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Parser where

import           Data.List (stripPrefix)

data DataTarget
    = TotalStars
    | MostUsedLang
    | Repos
    | Followers
    | UnspecifiedTarget
    deriving (Show)

data AppConfig = AppConfig
    { cfgUser     :: Maybe String
    , cfgTarget   :: DataTarget
    , cfgToken    :: Maybe String
    , cfgFormat   :: String
    , cfgLimit    :: Int
    , cfgShowHelp :: Bool
    , cfgShowVer  :: Bool
    }
    deriving (Show)

defaultConfig :: AppConfig
defaultConfig = AppConfig
    { cfgUser     = Nothing
    , cfgTarget   = UnspecifiedTarget
    , cfgToken    = Nothing
    , cfgFormat   = "table"
    , cfgLimit    = 1000
    , cfgShowHelp = False
    , cfgShowVer  = False
    }

parseArgs :: [String] -> AppConfig -> AppConfig
parseArgs [] config = config
parseArgs (arg : rest) config =
    case arg of
        "--help"        -> parseArgs rest (config { cfgShowHelp = True })
        "--version"     -> parseArgs rest (config { cfgShowVer = True })
        "--total-stars" -> parseArgs rest (config { cfgTarget = TotalStars })
        "--repos"       -> parseArgs rest (config { cfgTarget = Repos })
        _
            | Just username <- stripPrefix "--user=" arg  ->
                parseArgs rest (config { cfgUser = Just username })
            | Just token    <- stripPrefix "--token=" arg ->
                parseArgs rest (config { cfgToken = Just token })
            | Just format   <- stripPrefix "--format=" arg ->
                parseArgs rest (config { cfgFormat = format })
            | Just limitStr <- stripPrefix "--limit=" arg ->
                parseArgs rest (config { cfgLimit = read limitStr })
            | otherwise                                   ->
                parseArgs rest config
