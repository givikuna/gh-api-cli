module GitHub.Engine where

import           Data.List    (sortOn)
import           Data.Maybe   (fromMaybe)
import           Data.Ord     (Down (..))

import           CLI.Parser   (AppConfig (..), SortOrder (..))

import           GitHub.Types (Repo (..))

processRepos :: AppConfig -> [Repo] -> [Repo]
processRepos config repos =
    let
        noExcludedLangs = if null (cfgExcludeLangs config)
            then repos
            else
                filter (\r -> maybe True (\l -> not (l `elem` cfgExcludeLangs config)) (language r)) repos

        noExcludedMain = if null (cfgExcludeMainLang config)
            then noExcludedLangs
            else
                filter (\r -> maybe True (\l -> not (l `elem` cfgExcludeMainLang config)) (language r)) noExcludedLangs

        filteredForks = case cfgFilterFork config of
                Nothing    -> noExcludedMain
                Just True  -> filter fork noExcludedMain
                Just False -> filter (\r -> not (fork r)) noExcludedMain

        filteredArchived = case cfgFilterArchived config of
            Nothing    -> filteredForks
            Just True  -> filter archived filteredForks
            Just False -> filter (\r -> not (archived r)) filteredForks

        filteredNameSub = case cfgFilterNameSubstr config of
            Nothing   -> filteredArchived
            Just sub  -> filter (\r -> sub `isInfixOf` name r) filteredArchived
              where isInfixOf s t = any (s `isPrefixOf`) (tails t)
                    isPrefixOf [] _          = True
                    isPrefixOf _ []          = False
                    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
                    tails []         = [[]]
                    tails xxs@(_:xs) = xxs : tails xs

        filteredLangs = if null (cfgFilterLangs config)
            then filteredNameSub
            else filter (\r -> maybe False (\l -> l `elem` cfgFilterLangs config) (language r)) filteredNameSub

        filteredMainLang = case cfgFilterMainLang config of
            Nothing   -> filteredLangs
            Just lang -> filter (\r -> maybe False (== lang) (language r)) filteredLangs

        sortField = fromMaybe "stars" (cfgSort config)
        orderDir  = fromMaybe Desc (cfgOrder config)

        applyOrder :: Ord b => (Repo -> b) -> [Repo] -> [Repo]
        applyOrder selector lst = case orderDir of
            Asc  -> sortOn selector lst
            Desc -> sortOn (\r -> Down (selector r)) lst

        sortedRepos = case sortField of
            "name"    -> sortOn name filteredMainLang
            "size"    -> applyOrder size filteredMainLang
            "forks"   -> applyOrder forksCount filteredMainLang
            "created" -> applyOrder createdAt filteredMainLang
            "updated" -> applyOrder updatedAt filteredMainLang
            "pushed"  -> applyOrder pushedAt filteredMainLang
            _         -> applyOrder stargazersCount filteredMainLang

        limitedRepos = take (cfgLimit config) sortedRepos

    in limitedRepos
