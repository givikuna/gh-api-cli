{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main (main) where

import           Data.FileEmbed     (embedStringFile)

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           CLI.Parser         (AppConfig (..), DataTarget (..), defaultConfig, parseArgs)

import           GitHub.Client
    (getFollowers, getFollowing, getOrgs, getRepoLanguages, getRepos, getStarredRepos, getUser)
import           GitHub.Engine
    (getLanguageStats, getTotalStars, processOrgs, processRepos, processUsers)
import           GitHub.Types       (LangStat, Org (..), Repo (..), User (..))

import           Data.Map           (Map)
import           Data.Maybe         (isJust)
import           Output.Format      (printOrgs, printRepos, printStats, printUsers)

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
            case cfgTarget config of
                UnspecifiedTarget -> do
                    result <- getUser username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to parse GitHub response"
                            putStrLn err
                            exitFailure
                        Right user -> printUsers config [user]

                Repos -> do
                    result <- getRepos (cfgLimit config) username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos :: [Repo] = processRepos config rawRepos
                            printRepos config finalRepos

                MostUsedLang -> do
                    result <- getRepos (max 1000 (cfgLimit config)) username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let allReposConfig = config { cfgLimit = maxBound }
                            let finalRepos :: [Repo] = processRepos allReposConfig rawRepos
                            langResults <- mapM (\r -> getRepoLanguages (languagesUrl r) (cfgToken config)) finalRepos
                            let validMaps :: [Map String Int] = [m | Right m <- langResults]
                            let stats :: [LangStat] = getLanguageStats config validMaps
                            printStats config stats

                Stars -> do
                    result <- getStarredRepos (cfgLimit config) username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch starred repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let finalRepos :: [Repo] = processRepos config rawRepos
                            printRepos config finalRepos

                Followers -> do
                    result <- getFollowers (cfgLimit config) username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right rawUsers -> do
                            let needsDeep = isJust (cfgFilterCompany config) || cfgSort config `elem` [Just "followers", Just "repos"]
                            usersToProcess <- if needsDeep
                                then mapM (\u@(User { login = l }) -> do
                                    res <- getUser l (cfgToken config)
                                    case res of
                                        Right fullUser -> return fullUser
                                        Left _         -> return u
                                ) rawUsers
                                else return rawUsers

                            let finalUsers :: [User] = processUsers config usersToProcess
                            printUsers config finalUsers

                Following -> do
                    result <- getFollowing (cfgLimit config) username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right rawUsers -> do
                            let needsDeep = isJust (cfgFilterCompany config) || cfgSort config `elem` [Just "followers", Just "repos"]
                            usersToProcess <- if needsDeep
                                then mapM (\u@(User { login = l }) -> do
                                    res <- getUser l (cfgToken config)
                                    case res of
                                        Right fullUser -> return fullUser
                                        Left _         -> return u
                                ) rawUsers
                                else return rawUsers

                            let finalUsers :: [User] = processUsers config usersToProcess
                            printUsers config finalUsers

                Orgs -> do
                    result <- getOrgs (cfgLimit config) username $ cfgToken config
                    case result of
                        Left err -> putStrLn err >> exitFailure
                        Right rawOrgs -> do
                            let finalOrgs :: [Org] = processOrgs config rawOrgs
                            printOrgs config finalOrgs

                TotalStars -> do
                    -- Fetch maximum threshold to calculate stars properly, bypass visual limit
                    result <- getRepos (max 1000 (cfgLimit config)) username $ cfgToken config
                    case result of
                        Left err -> do
                            putStrLn "ERROR: failed to fetch repos"
                            putStrLn err
                            exitFailure
                        Right rawRepos -> do
                            let allReposConfig = config { cfgLimit = maxBound }
                            let finalRepos :: [Repo] = processRepos allReposConfig rawRepos
                            let total :: Int = getTotalStars finalRepos
                            if cfgFormat config == "json"
                                then putStrLn $ "{\"total_stars\": " ++ show total ++ "}"
                                else putStrLn $ "Total stars: " ++ show total
