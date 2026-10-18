{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Output.Format where

import           CLI.Parser                 (AppConfig (..))
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           GitHub.Types               (LangStat (..), Org (..), Repo (..), User (..))
import           Text.Printf                (printf)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

printRepos :: AppConfig -> [Repo] -> IO ()
printRepos config repos = case cfgFormat config of
    "json"  -> BSL.putStrLn $ encode repos
    "table" -> do
        putStrLn $ padRight 40 "NAME" ++ padRight 15 "STARS" ++ "LANGUAGE"
        putStrLn $ replicate 75 '-'
        mapM_ printRepoRow repos
    _       -> putStrLn "Unknown format"
  where
    printRepoRow r =
        let lang :: String = case language r of
                    Just l  -> l
                    Nothing -> "N/A"
        in putStrLn $ padRight 40 (take 38 $ name r)
                   ++ padRight 15 (show $ stargazersCount r)
                   ++ lang

printUsers :: AppConfig -> [User] -> IO ()
printUsers config users = case cfgFormat config of
    "json"  -> BSL.putStrLn $ encode users
    "table" -> do
        putStrLn $ padRight 30 "LOGIN" ++ padRight 15 "FOLLOWERS" ++ "COMPANY"
        putStrLn $ replicate 70 '-'
        mapM_ printUserRow users
    _       -> putStrLn "Unknown format"
  where
    printUserRow (User { login = l, followers = f, company = c }) =
        let fols :: String  = maybe "N/A" show f
            comp = maybe "N/A" id c
        in putStrLn $ padRight 30 (take 28 l)
                   ++ padRight 15 fols
                   ++ comp

printOrgs :: AppConfig -> [Org] -> IO ()
printOrgs config orgs = case cfgFormat config of
    "json"  -> BSL.putStrLn $ encode orgs
    "table" -> do
        putStrLn $ padRight 30 "LOGIN" ++ "DESCRIPTION"
        putStrLn $ replicate 60 '-'
        mapM_ printOrgRow orgs
    _       -> putStrLn "Unknown format"
  where
    printOrgRow (Org { login = l, description = d }) =
        let desc :: String  = maybe "N/A" (take 50) d
        in putStrLn $ padRight 30 (take 28 l) ++ desc

printStats :: AppConfig -> [LangStat] -> IO ()
printStats config stats = case cfgFormat config of
    "json"  -> BSL.putStrLn $ encode stats
    "table" -> do
        putStrLn $ padRight 25 "LANGUAGE" ++ padRight 15 "PERCENTAGE" ++ "BYTES"
        putStrLn $ replicate 60 '-'
        mapM_ printStatRow stats
    _       -> putStrLn "Unknown format"
  where
    printStatRow stat =
        putStrLn $ padRight 25 (take 23 $ langName stat)
                ++ padRight 15 (printf "%.2f%%" (langPerc stat) :: String)
                ++ show (langBytes stat)
