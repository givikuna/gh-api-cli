{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

-- import           Data.List          (group, sort)

import           Data.FileEmbed     (embedStringFile)

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           CLI.Parser         (AppConfig (..), DataTarget (..), defaultConfig, parseArgs)

import           GitHub.Client      (getRepos, getUser)
import           GitHub.Engine      (getLanguageStats, getTotalStars, processRepos)
import           GitHub.Types       (LangStat (..), Repo (..))

appVersion :: String
appVersion = $(embedStringFile ".version")

main :: IO ()
main = do
    args :: [String] <- getArgs
    let config :: AppConfig = parseArgs args defaultConfig

    if cfgShowHelp config then do
        putStrLn "usage: gh-api-cli --user<username> [TARGET] [OPTIONs]"
        putStrLn "targets: --total-stars, --repos, --followers, --most-used-lang"

    else if cfgShowVer config then do
        putStrLn $ "gh-api-cli v" ++ filter (/= '\n') appVersion

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
                    putStrLn "fetching repos..."
                    result <- getRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: Failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos :: [Repo] = processRepos config rawRepos
                            putStrLn $ "Found " ++ show (length finalRepos) ++ " repos!"

                            mapM_ (\r ->
                                    putStrLn $ name r ++ " - Stars: " ++ show (stargazersCount r)) finalRepos
                MostUsedLang -> do
                    putStrLn "fetching repos to calculate language statistics..."
                    result <- getRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: Failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos = processRepos config rawRepos
                            let stats = getLanguageStats config finalRepos
                            putStrLn $ "language stats based on " ++ show (length finalRepos) ++ " repos:"
                            mapM_ (\stat ->
                                putStrLn $ langName stat ++ ": " ++ show (langCount stat) ++ " repos") stats
                Stars -> do
                    putStrLn "..."
                Followers -> do
                    putStrLn "..."
                Following -> do
                    putStrLn "..."
                Orgs -> do
                    putStrLn "..."
                TotalStars -> do
                    putStrLn "fetching repos to calculate total stars..."
                    result <- getRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos :: [Repo] = processRepos config rawRepos
                            let total :: Int = getTotalStars finalRepos
                            putStrLn $ "total stars (across " ++ show (length finalRepos) ++ " repos): " ++ show total
