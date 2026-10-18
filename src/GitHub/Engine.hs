{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitHub.Engine where

import           Data.List        (isPrefixOf, sortOn, tails)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromMaybe)
import           Data.Ord         (Down (..))
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)
import           Text.Regex.TDFA  ((=~))

import           CLI.Parser       (AppConfig (..), SortOrder (..))
import           GitHub.Types     (LangStat (..), Org (..), Repo (..), User (..))

isSubstrOf :: String -> String -> Bool
isSubstrOf sub str = any (sub `isPrefixOf`) (tails str)

parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

processRepos :: AppConfig -> [Repo] -> [Repo]
processRepos config repos =
    let
        fIgnore r = not $ name r `elem` cfgIgnoreRepos config
        fExcludeLangs r = maybe True (`notElem` cfgExcludeLangs config) (language r)
        fExcludeMain r  = maybe True (`notElem` cfgExcludeMainLang config) (language r)
        fExcludeXor r   = maybe True (`notElem` cfgExcludeXorLang config) (language r)

        fFork r = case cfgFilterFork config of
            Nothing -> True
            Just b  -> fork r == b

        fArchived r = case cfgFilterArchived config of
            Nothing -> True
            Just b  -> archived r == b

        fNameSub r = case cfgFilterNameSubstr config of
            Nothing -> True
            Just s  -> s `isSubstrOf` name r

        fNameReg r = case cfgFilterNameRegex config of
            Nothing  -> True
            Just reg -> name r =~ reg

        fVis r = case cfgFilterVisibility config of
            "all" -> True
            v     -> visibility r == v

        fCreatedAfter r = case cfgFilterCreatedAfter config >>= parseDate of
            Nothing -> True
            Just d  -> createdAt r >= d

        fCreatedBefore r = case cfgFilterCreatedBefore config >>= parseDate of
            Nothing -> True
            Just d  -> createdAt r <= d

        fUpdatedAfter r = case cfgFilterUpdatedAfter config >>= parseDate of
            Nothing -> True
            Just d  -> updatedAt r >= d

        fUpdatedBefore r = case cfgFilterUpdatedBefore config >>= parseDate of
            Nothing -> True
            Just d  -> updatedAt r <= d

        fLangs r = if null (cfgFilterLangs config)
            then True
            else maybe False (`elem` cfgFilterLangs config) (language r)

        fMainLang r = case cfgFilterMainLang config of
            Nothing -> True
            Just l  -> maybe False (== l) (language r)

        allFilters r = fIgnore r && fExcludeLangs r && fExcludeMain r && fExcludeXor r &&
                       fFork r && fArchived r && fNameSub r && fNameReg r && fVis r &&
                       fCreatedAfter r && fCreatedBefore r && fUpdatedAfter r && fUpdatedBefore r &&
                       fLangs r && fMainLang r

        filtered = filter allFilters repos

        sortField = fromMaybe "stars" (cfgSort config)
        orderDir  = case cfgOrder config of
            Just o  -> o
            Nothing -> if sortField `elem` ["name", "created"] then Asc else Desc

        applyOrder :: Ord b => (Repo -> b) -> [Repo] -> [Repo]
        applyOrder selector lst = case orderDir of
            Asc  -> sortOn selector lst
            Desc -> sortOn (\r -> Down (selector r)) lst

        sortedRepos = case sortField of
            "name"       -> applyOrder name filtered
            "size"       -> applyOrder size filtered
            "forks"      -> applyOrder forksCount filtered
            "created"    -> applyOrder createdAt filtered
            "updated"    -> applyOrder updatedAt filtered
            "pushed"     -> applyOrder pushedAt filtered
            "starred_at" -> case orderDir of
                              Asc  -> reverse filtered
                              Desc -> filtered
            _            -> applyOrder stargazersCount filtered

    in take (cfgLimit config) sortedRepos

processUsers :: AppConfig -> [User] -> [User]
processUsers config users =
    let fNameSub (User { login = l }) = case cfgFilterNameSubstr config of
            Nothing -> True
            Just s  -> s `isSubstrOf` l

        fNameReg (User { login = l }) = case cfgFilterNameRegex config of
            Nothing  -> True
            Just reg -> l =~ reg

        fComp u = case cfgFilterCompany config of
            Nothing -> True
            Just s  -> maybe False (\c -> s `isSubstrOf` c) (company u)

        allFilters u = fNameSub u && fNameReg u && fComp u
        filteredUsers = filter allFilters users

        sortField :: String = fromMaybe "name" (cfgSort config)
        orderDir  :: SortOrder = case cfgOrder config of
            Just o  -> o
            Nothing -> if sortField == "name" then Asc else Desc

        applyOrder :: Ord b => (User -> b) -> [User] -> [User]
        applyOrder selector lst = case orderDir of
            Asc  -> sortOn selector lst
            Desc -> sortOn (\u -> Down (selector u)) lst

        sortedUsers :: [User] = case sortField of
            "followers" -> applyOrder (fromMaybe 0 . followers) filteredUsers
            "repos"     -> applyOrder (fromMaybe 0 . publicRepos) filteredUsers
            "joined"    -> case orderDir of
                             Asc  -> reverse filteredUsers
                             Desc -> filteredUsers
            _           -> applyOrder (\(User { login = l }) -> l) filteredUsers

    in take (cfgLimit config) sortedUsers

processOrgs :: AppConfig -> [Org] -> [Org]
processOrgs config orgs =
    let fNameSub (Org { login = l }) = case cfgFilterNameSubstr config of
            Nothing -> True
            Just s  -> s `isSubstrOf` l

        filteredOrgs = filter fNameSub orgs

        sortField :: String = fromMaybe "name" (cfgSort config)
        orderDir  :: SortOrder = case cfgOrder config of
            Just o  -> o
            Nothing -> if sortField == "name" then Asc else Desc

        sortedOrgs = case sortField of
            _ -> case orderDir of
                    Asc  -> sortOn (\(Org { login = l }) -> l) filteredOrgs
                    Desc -> sortOn (\(Org { login = l }) -> Down l) filteredOrgs

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
