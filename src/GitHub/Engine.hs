{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GitHub.Engine where

import           Control.Monad    (filterM)

import           Data.Char        (toLower)
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

processRepos :: Monad m => AppConfig -> (String -> m (Either String (Map.Map String Int))) -> [Repo] -> m [Repo]
processRepos config fetchLangs repos = do
    let
        fIgnore r = not $ name r `elem` cfgIgnoreRepos config
        fExcludeLangs r = maybe True (`notElem` cfgExcludeLangs config) (language r)
        fExcludeMain r  = maybe True (`notElem` cfgExcludeMainLang config) (language r)

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

        fCreatedAfter (Repo { createdAt = c }) = case cfgFilterCreatedAfter config >>= parseDate of
            Nothing -> True
            Just d  -> c >= d

        fCreatedBefore (Repo { createdAt = c }) = case cfgFilterCreatedBefore config >>= parseDate of
            Nothing -> True
            Just d  -> c <= d

        fUpdatedAfter r = case cfgFilterUpdatedAfter config >>= parseDate of
            Nothing -> True
            Just d  -> updatedAt r >= d

        fUpdatedBefore r = case cfgFilterUpdatedBefore config >>= parseDate of
            Nothing -> True
            Just d  -> updatedAt r <= d

        fMainLang r = case cfgFilterMainLang config of
            Nothing -> True
            Just l  -> maybe False (== l) (language r)

        allPureFilters r = fIgnore r && fExcludeLangs r && fExcludeMain r &&
                       fFork r && fArchived r && fNameSub r && fNameReg r && fVis r &&
                       fCreatedAfter r && fCreatedBefore r && fUpdatedAfter r && fUpdatedBefore r &&
                       fMainLang r

        pureFiltered = filter allPureFilters repos

    let needsDeep = not (null (cfgFilterLangs config)) || not (null (cfgExcludeXorLang config))

    deepFiltered <- if not needsDeep
        then return pureFiltered
        else filterM (\r -> do
            res <- fetchLangs (languagesUrl r)
            case res of
                Right lMap -> do
                    let fLangs = if null (cfgFilterLangs config)
                            then True
                            else any (`Map.member` lMap) (cfgFilterLangs config)

                    let fXor = case cfgExcludeXorLang config of
                            [l1, l2] -> Map.member l1 lMap == Map.member l2 lMap
                            _        -> True

                    return $ fLangs && fXor
                Left _ -> return False
        ) pureFiltered

    let sortField = fromMaybe "stars" (cfgSort config)
    let orderDir  = case cfgOrder config of
            Just o  -> o
            Nothing -> if sortField `elem` ["name", "created"] then Asc else Desc

    let applyOrder :: Ord b => (Repo -> b) -> [Repo] -> [Repo]
        applyOrder selector lst = case orderDir of
            Asc  -> sortOn selector lst
            Desc -> sortOn (\r -> Down (selector r)) lst

    let sortedRepos = case sortField of
            "name"    -> applyOrder name deepFiltered
            "size"    -> applyOrder size deepFiltered
            "forks"   -> applyOrder forksCount deepFiltered
            "created" -> applyOrder (\(Repo { createdAt = c }) -> c) deepFiltered
            "updated" -> applyOrder updatedAt deepFiltered
            "pushed"  -> applyOrder pushedAt deepFiltered
            _         -> applyOrder stargazersCount deepFiltered

    return $ take (cfgLimit config) sortedRepos

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
            "joined"    -> applyOrder (\(User { createdAt = c }) -> c) filteredUsers
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
    rawCombinedMap = Map.unionsWith (+) langMaps

    excludes = map (map toLower) (cfgExcludeLangs config)

    combinedMap = Map.filterWithKey (\k _ -> map toLower k `notElem` excludes) rawCombinedMap

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
