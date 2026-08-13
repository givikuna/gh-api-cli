{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           CLI.Parser         (AppConfig (..), DataTarget (..), defaultConfig, parseArgs)

import           GitHub.Client      (getRepos, getUser)
import           GitHub.Engine      (processRepos)
import           GitHub.Types       (Repo (..))

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

            case cfgTarget config of
                UnspecifiedTarget -> do
                    result <- getUser username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: Failed to parse GitHub response"
                            putStrLn err
                            exitFailure
                        Right user -> do
                            putStrLn "YAY the USER has been FETCHED"
                            print user
                Repos -> do
                    putStrLn "Fetching repos..."
                    result <- getRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: Failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos = processRepos config rawRepos
                            putStrLn $ "Found " ++ show (length finalRepos) ++ " repos!"

                            mapM_ (\r ->
                                    putStrLn $ name r ++ " - Stars: " ++ show (stargazersCount r)) finalRepos

                _ -> do
                    putStrLn "ERROR: What you look for does not exist"
                    exitFailure

