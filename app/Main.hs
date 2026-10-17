{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import           Data.FileEmbed     (embedStringFile)
import qualified Data.Map           as Map

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           CLI.Parser         (AppConfig (..), DataTarget (..), defaultConfig, parseArgs)

import           GitHub.Client
    (getFollowers, getFollowing, getOrgs, getRepoLanguages, getRepos, getStarredRepos, getUser)
import           GitHub.Engine      (getLanguageStats, getTotalStars, processRepos)
import           GitHub.Types       (LangStat (..), Org (..), Repo (..), User (..))

import           Text.Printf

appVersion :: String
appVersion = $(embedStringFile ".version")

main :: IO ()
main = do
    args :: [String] <- getArgs
    let config :: AppConfig = parseArgs args defaultConfig

    if cfgShowHelp config then do
        putStrLn "usage: gh-api-cli --user<username> [TARGET] [OPTIONs]"
        putStrLn "targets: --total-stars, --repos, --stars, --followers, --following, --orgs, --most-used-lang"

    else if cfgShowVer config then do
        putStrLn $ "gh-api-cli v" ++ filter (/= '\n') appVersion

    else
        case cfgUser config of
        Nothing -> do
            putStrLn "ERROR: provide an username. example: --user=KrakenTheJacken"
            exitFailure

        Just username -> do
            putStrLn $ "target acquired: " ++ username

            case cfgTarget config of
                UnspecifiedTarget -> do
                    result <- getUser username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to parse GitHub response"
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
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos :: [Repo] = processRepos config rawRepos
                            putStrLn $ "Found " ++ show (length finalRepos) ++ " repos!"
                            mapM_ (\r -> putStrLn $ name r ++ " - Stars: " ++ show (stargazersCount r)) finalRepos

                MostUsedLang -> do
                    putStrLn "fetching repos to calculate real language statistics..."
                    result <- getRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos = processRepos config rawRepos
                            putStrLn $ "fetching actual line/byte counts for " ++ show (length finalRepos) ++ " repos. This may take a moment..."

                            langResults <- mapM (\r -> getRepoLanguages (languagesUrl r) (cfgToken config)) finalRepos

                            let validMaps = [m | Right m <- langResults]
                            let stats = getLanguageStats config validMaps

                            putStrLn $ "Language stats (by actual code size) across " ++ show (length finalRepos) ++ " repos:"
                            mapM_ (\stat ->
                                putStrLn $ langName stat ++ ": "
                                        ++ printf "%.2f%%" (langPerc stat)
                                        ++ " (" ++ show (langBytes stat) ++ " bytes)"
                                ) stats

                Stars -> do
                    putStrLn "fetching starred repos..."
                    result <- getStarredRepos username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: Failed to fetch starred repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos = processRepos config rawRepos
                            putStrLn $ "found " ++ show (length finalRepos) ++ " starred repos!"
                            mapM_ (\r -> putStrLn $ name r ++ " - Stars: " ++ show (stargazersCount r)) finalRepos

                Followers -> do
                    putStrLn "fetching followers..."
                    result <- getFollowers username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right users -> do
                            putStrLn $ "found " ++ show (length users) ++ " followers!"
                            mapM_ (\u -> putStrLn $ login (u :: User)) users

                Following -> do
                    putStrLn "fetching following..."
                    result <- getFollowing username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right users -> do
                            putStrLn $ "following " ++ show (length users) ++ " users!"
                            mapM_ (\u -> putStrLn $ login (u :: User)) users

                Orgs -> do
                    putStrLn "fetching organizations..."
                    result <- getOrgs username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right orgsList -> do
                            putStrLn $ "found " ++ show (length orgsList) ++ " organizations!"
                            mapM_ (\o -> putStrLn $ login (o :: Org)) orgsList

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
