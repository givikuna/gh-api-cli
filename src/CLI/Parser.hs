{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Parser where

import Data.List (stripPrefix)

data Command
    = Help
    | Version
    | GetUser String
    | Unknown String
    deriving (Show)

parseArgs :: [String] -> Command
parseArgs [] = Help
parseArgs ("--help" : _) = Help
parseArgs ("--version" : _) = Version
parseArgs ((arg :: String) : (_ :: [String])) =
    let maybeUsername :: Maybe String = stripPrefix "--user=" arg
     in case maybeUsername of
            Just (username :: String) -> GetUser username
            Nothing -> Unknown arg
