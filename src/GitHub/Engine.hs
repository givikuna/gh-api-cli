module GitHub.Engine where

import           Data.List    (sortOn)
import           Data.Ord     (Down (..))

import           CLI.Parser   (AppConfig (..))

import           GitHub.Types (Repo (..))

processRepos :: AppConfig -> [Repo] -> [Repo]
processRepos config repos =
    let
        noForks = filter (\r -> not (fork r)) repos
        sortedRepos = sortOn (\r -> Down (stargazersCount r)) noForks
        limitedRepos = take (cfgLimit config) sortedRepos
    in limitedRepos
