{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Parser where

import           Data.List (stripPrefix)

data DataTarget
    = TotalStars
    | MostUsedLang
    | Repos
    | Stars
    | Followers
    | Following
    | Orgs
    | UnspecifiedTarget
    deriving (Show, Eq)

data SortOrder = Asc | Desc deriving (Show, Eq)

data AppConfig = AppConfig
    { cfgUser                :: Maybe String
    , cfgTarget              :: DataTarget
    , cfgToken               :: Maybe String
    , cfgFormat              :: String
    , cfgLimit               :: Int
    , cfgShowHelp            :: Bool
    , cfgShowVer             :: Bool

    --

    , cfgSort                :: Maybe String
    , cfgOrder               :: Maybe SortOrder

    --

    , cfgExcludeLangs        :: [String]
    , cfgExcludeMainLang     :: [String]
    , cfgExcludeXorLang      :: [String]

    , cfgFilterLangs         :: [String]
    , cfgFilterMainLang      :: Maybe String

    --

    , cfgFilterNameSubstr    :: Maybe String
    , cfgFilterNameRegex     :: Maybe String
    , cfgFilterCompany       :: Maybe String

    --

    , cfgFilterCreatedAfter  :: Maybe String
    , cfgFilterCreatedBefore :: Maybe String
    , cfgFilterUpdatedAfter  :: Maybe String
    , cfgFilterUpdatedBefore :: Maybe String

    --

    , cfgFilterVisibility    :: String
    , cfgFilterFork          :: Maybe Bool
    , cfgFilterArchived      :: Maybe Bool
    } deriving (Show)

defaultConfig :: AppConfig
defaultConfig = AppConfig
    { cfgUser                = Nothing
    , cfgTarget              = UnspecifiedTarget
    , cfgToken               = Nothing
    , cfgFormat              = "table"
    , cfgLimit               = 1000
    , cfgShowHelp            = False
    , cfgShowVer             = False

    , cfgSort                = Nothing
    , cfgOrder               = Nothing

    , cfgExcludeLangs        = []
    , cfgExcludeMainLang     = []
    , cfgExcludeXorLang      = []
    , cfgFilterLangs         = []
    , cfgFilterMainLang      = Nothing

    , cfgFilterNameSubstr    = Nothing
    , cfgFilterNameRegex     = Nothing
    , cfgFilterCompany       = Nothing

    , cfgFilterCreatedAfter  = Nothing
    , cfgFilterCreatedBefore = Nothing
    , cfgFilterUpdatedAfter  = Nothing
    , cfgFilterUpdatedBefore = Nothing

    , cfgFilterVisibility    = "all"
    , cfgFilterFork          = Nothing
    , cfgFilterArchived      = Nothing
    }

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delim str =
    let (start, rest) :: ([Char], [Char]) = break (== delim) str
        (_, remain) = span (== delim) rest
    in start : splitBy delim remain

parseArgs :: [String] -> AppConfig -> AppConfig
parseArgs [] config = config
parseArgs (arg : rest) config =
    case arg of
        "--help"            -> parseArgs rest (config { cfgShowHelp = True })
        "--version"         -> parseArgs rest (config { cfgShowVer = True })

        "--total-stars"     -> parseArgs rest (config { cfgTarget = TotalStars })
        "--most-used-lang"  -> parseArgs rest (config { cfgTarget = MostUsedLang })
        "--repos"           -> parseArgs rest (config { cfgTarget = Repos })
        "--stars"           -> parseArgs rest (config { cfgTarget = Stars })
        "--followers"       -> parseArgs rest (config { cfgTarget = Followers })
        "--following"       -> parseArgs rest (config { cfgTarget = Following })
        "--orgs"            -> parseArgs rest (config { cfgTarget = Orgs })

        "-a" -> parseArgs rest (config { cfgLimit = maxBound })
        "-x" -> parseArgs rest (config { cfgLimit = 10 })
        "-1" -> parseArgs rest (config { cfgLimit = 1 })
        "-2" -> parseArgs rest (config { cfgLimit = 2 })
        "-3" -> parseArgs rest (config { cfgLimit = 3 })
        "-4" -> parseArgs rest (config { cfgLimit = 4 })
        "-5" -> parseArgs rest (config { cfgLimit = 5 })
        "-6" -> parseArgs rest (config { cfgLimit = 6 })
        "-7" -> parseArgs rest (config { cfgLimit = 7 })
        "-8" -> parseArgs rest (config { cfgLimit = 8 })
        "-9" -> parseArgs rest (config { cfgLimit = 9 })

        "--order=asc"               -> parseArgs rest (config { cfgOrder = Just Asc })
        "--order=desc"              -> parseArgs rest (config { cfgOrder = Just Desc })
        "--filter-fork=true"        -> parseArgs rest (config { cfgFilterFork = Just True })
        "--filter-fork=false"       -> parseArgs rest (config { cfgFilterFork = Just False })
        "--filter-archived=true"    -> parseArgs rest (config { cfgFilterArchived = Just True })
        "--filter-archived=false"   -> parseArgs rest (config { cfgFilterArchived = Just False })

        _
            | Just username <- stripPrefix "--user=" arg
            -> parseArgs rest $ config
                { cfgUser = Just username }

            | Just token <- stripPrefix "--token=" arg
            -> parseArgs rest $ config
                { cfgToken = Just token }

            | Just format <- stripPrefix "--format=" arg
            -> parseArgs rest $ config
                { cfgFormat = format }

            | Just limitStr <- stripPrefix "--limit=" arg
            -> parseArgs rest $ config
                { cfgLimit = read limitStr }

            | Just sortStr <- stripPrefix "--sort=" arg
            -> parseArgs rest $ config
                { cfgSort = Just sortStr }

            | Just exclude <- stripPrefix "--exclude=" arg
            -> parseArgs rest $ config
                { cfgExcludeLangs = cfgExcludeLangs config ++ splitBy ',' exclude }

            | Just excludeLangs <- stripPrefix "--exclude-lang=" arg
            -> parseArgs rest $ config
                { cfgExcludeLangs = cfgExcludeLangs config ++ splitBy ',' excludeLangs }

            | Just excludeMain <- stripPrefix "--exclude-main-lang=" arg
            -> parseArgs rest $ config
                { cfgExcludeMainLang = cfgExcludeMainLang config ++ splitBy ',' excludeMain }

            | Just excludeXor <- stripPrefix "--exclude-xor-lang=" arg
            -> parseArgs rest $ config
                { cfgExcludeXorLang = cfgExcludeXorLang config ++ splitBy ',' excludeXor }

            | Just nameSub <- stripPrefix "--filter-name-substr=" arg
            -> parseArgs rest $ config
                { cfgFilterNameSubstr = Just nameSub }

            | Just nameReg <- stripPrefix "--filter-name=" arg
            -> parseArgs rest $ config
                { cfgFilterNameRegex = Just nameReg }

            | Just company <- stripPrefix "--filter-company=" arg
            -> parseArgs rest $ config
                { cfgFilterCompany = Just company }

            | Just cAfter <- stripPrefix "--filter-created-after=" arg
            -> parseArgs rest $ config
                { cfgFilterCreatedAfter = Just cAfter }

            | Just cBefore <- stripPrefix "--filter-created-before=" arg
            -> parseArgs rest $ config
                { cfgFilterCreatedBefore = Just cBefore }

            | Just uAfter <- stripPrefix "--filter-updated-after=" arg
            -> parseArgs rest $ config
                { cfgFilterUpdatedAfter = Just uAfter }

            | Just uBefore <- stripPrefix "--filter-updated-before=" arg
            -> parseArgs rest $ config
                { cfgFilterUpdatedBefore = Just uBefore }

            -- Enums & Language Filters
            | Just vis <- stripPrefix "--filter-updated-visibility=" arg
            -> parseArgs rest $ config
                { cfgFilterVisibility = vis }

            | Just fLangs <- stripPrefix "--filter-language=" arg
            -> parseArgs rest $ config
                { cfgFilterLangs = cfgFilterLangs config ++ splitBy ',' fLangs }

            | Just fMainLang <- stripPrefix "--filter-main-language=" arg
            -> parseArgs rest $ config
                { cfgFilterMainLang = Just fMainLang }

            | otherwise
            -> parseArgs rest config
