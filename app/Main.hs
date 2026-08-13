{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           CLI.Parser         (AppConfig (..), defaultConfig, parseArgs)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)


main :: IO ()
main = do
    args :: [String] <- getArgs
    let config :: AppConfig = parseArgs args defaultConfig
    if cfgShowHelp config then do
        putStrLn "Usage: gh-api-cli --user<username> [TARGET] [OPTIONs]"
        putStrLn "Targets: --total-stars, --repos, --followers, --most-used-lang"
    else if cfgShowVer config then do
        putStrLn "gh-api-cli v0.1.0.0"
    else
        case cfgUser config of
        Nothing -> do
            putStrLn "ERROR: Provide an username. Example: --user=KrakenTheJacken"
            exitFailure
        Just username -> do
            putStrLn $ "Target acquired: " ++ username
            putStrLn $ "Action requested: " ++ show (cfgTarget config)
