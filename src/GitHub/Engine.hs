{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.Engine where

import           Data.List       (isPrefixOf, sortOn, tails)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Ord        (Down (..))

import           CLI.Parser      (AppConfig (..), SortOrder (..))
import           GitHub.Types    (LangStat (..), Org (..), Repo (..), User (..))

isSubstrOf :: String -> String -> Bool
isSubstrOf sub str = any (sub `isPrefixOf`) (tails str)

processRepos :: AppConfig -> [Repo] -> [Repo]
processRepos config repos =
    let
        noExcludedLangs = if null (cfgExcludeLangs config)
            then repos
            else filter (\r -> maybe True (\l -> not (l `elem` cfgExcludeLangs config)) (language r)) repos

        noExcludedMain = if null (cfgExcludeMainLang config)
            then noExcludedLangs
            else filter (\r -> maybe True (\l -> not (l `elem` cfgExcludeMainLang config)) (language r)) noExcludedLangs

        filteredForks = case cfgFilterFork config of
                Nothing    -> noExcludedMain
                Just True  -> filter fork noExcludedMain
                Just False -> filter (\r -> not (fork r)) noExcludedMain

        filteredArchived = case cfgFilterArchived config of
            Nothing    -> filteredForks
            Just True  -> filter archived filteredForks
            Just False -> filter (\r -> not (archived r)) filteredForks

        filteredNameSub = case cfgFilterNameSubstr config of
            Nothing  -> filteredArchived
            Just sub -> filter (\r -> sub `isSubstrOf` name r) filteredArchived

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

    in take (cfgLimit config) sortedRepos

processUsers :: AppConfig -> [User] -> [User]
processUsers config users =
    let filteredNameSub = case cfgFilterNameSubstr config of
            Nothing  -> users
            Just sub -> filter (\(User { login = l }) -> sub `isSubstrOf` l) users

        filteredNameReg = case cfgFilterNameRegex config of
            Nothing  -> filteredNameSub
            Just sub -> filter (\(User { login = l }) -> sub `isSubstrOf` l) filteredNameSub

        filteredCompany = case cfgFilterCompany config of
            Nothing  -> filteredNameReg
            Just sub -> filter (\u -> maybe False (\c -> sub `isSubstrOf` c) (company u)) filteredNameReg

        sortField = fromMaybe "name" (cfgSort config)
        orderDir  = fromMaybe Asc (cfgOrder config)

        applyOrder :: Ord b => (User -> b) -> [User] -> [User]
        applyOrder selector lst = case orderDir of
            Asc  -> sortOn selector lst
            Desc -> sortOn (\u -> Down (selector u)) lst

        sortedUsers = case sortField of
            "followers" -> applyOrder (fromMaybe 0 . followers) filteredCompany
            "repos"     -> applyOrder (fromMaybe 0 . publicRepos) filteredCompany
            "joined"    -> filteredCompany
            _           -> applyOrder (\(User { login = l }) -> l) filteredCompany

    in take (cfgLimit config) sortedUsers

processOrgs :: AppConfig -> [Org] -> [Org]
processOrgs config orgs =
    let filteredNameSub = case cfgFilterNameSubstr config of
            Nothing  -> orgs
            Just sub -> filter (\(Org { login = l }) -> sub `isSubstrOf` l) orgs

        sortField = fromMaybe "name" (cfgSort config)
        orderDir  = fromMaybe Asc (cfgOrder config)

        sortedOrgs = case sortField of
            _ -> case orderDir of
                    Asc  -> sortOn (\(Org { login = l }) -> l) filteredNameSub
                    Desc -> sortOn (\(Org { login = l }) -> Down l) filteredNameSub

    in take (cfgLimit config) sortedOrgs

getTotalStars :: [Repo] -> Int
getTotalStars repos = sum $ map stargazersCount repos

getLanguageStats :: AppConfig -> [Map.Map String Int] -> [LangStat]
getLanguageStats config langMaps = take (cfgLimit config) sortedStats
  where
    combinedMap = Map.unionsWith (+) langMaps
    totalBytes = sum (Map.elems combinedMap)

    baseStats = if totalBytes == 0
        then []
        else [ LangStat lName bytes ((fromIntegral bytes / fromIntegral totalBytes) * 100.0)
             | (lName, bytes) <- Map.toList combinedMap ]

    sortField = fromMaybe "percentage" (cfgSort config)
    orderDir  = case cfgOrder config of
        Just o  -> o
        Nothing -> if sortField == "name" then Asc else Desc

    sortedStats = case (sortField, orderDir) of
        ("name", Asc)  -> sortOn langName baseStats
        ("name", Desc) -> sortOn (\s -> Down (langName s)) baseStats
        (_, Asc)       -> sortOn langPerc baseStats
        (_, Desc)      -> sortOn (\s -> Down (langPerc s)) baseStats
