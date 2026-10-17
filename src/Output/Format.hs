module Output.Format where

import           CLI.Parser                 (AppConfig (..))
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           GitHub.Types               (Repo (..))

printRepos :: AppConfig -> [Repo] -> IO ()
printRepos config repos = case cfgFormat config of
    "json"  -> BSL.putStrLn $ encode repos
    "table" -> do
        putStrLn $ padRight 30 "NAME" ++ padRight 10 "STARS" ++ "LANGUAGE"
        putStrLn $ replicate 60 '-'
        mapM_ printRepoRow repos
    _       -> putStrLn "Unknown format"

  where
    printRepoRow r =
        let lang = case language r of
                    Just l  -> l
                    Nothing -> "N/A"
        in putStrLn $ padRight 30 (take 28 $ name r)
                   ++ padRight 10 (show $ stargazersCount r)
                   ++ lang

    padRight :: Int -> String -> String
    padRight n s = s ++ replicate (n - length s) ' '
