{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHub.Client where

import           Data.Aeson                (FromJSON, eitherDecode)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.Map                  as Map

import           Network.HTTP.Client
    (Request, httpLbs, parseRequest, requestHeaders, responseBody)
import           Network.HTTP.Client.TLS   (newTlsManager)
import           Network.HTTP.Types        (HeaderName)
import           Network.HTTP.Types.Header (hAuthorization, hUserAgent)

import           GitHub.Types              (Org, Repo, User)


buildRequest :: String -> Maybe String -> IO Request
buildRequest url mToken = do
    initialRequest <- parseRequest url
    let baseHeaders :: [(HeaderName, ByteString)] = [(hUserAgent, "gh-api-cli")]
    let authHeaders :: [(HeaderName, ByteString)] = case mToken of
                Nothing    -> []
                Just token -> [(hAuthorization, BC.pack $ "Bearer " ++ token)]
    return initialRequest { requestHeaders = baseHeaders ++ authHeaders }

-- | Generic paginator to fetch until the limit or until no more results are found.
paginate :: FromJSON a => Int -> String -> Maybe String -> IO (Either String [a])
paginate limit baseUrl mToken = do
    manager <- newTlsManager
    go manager 1 []
  where
    go manager page acc
        | length acc >= limit = return $ Right (take limit acc)
        | otherwise = do
            let sep :: [Char] = if '?' `elem` baseUrl then "&" else "?"
            let url :: [Char] = baseUrl ++ sep ++ "page=" ++ show page ++ "&per_page=100"
            req <- buildRequest url mToken
            res <- httpLbs req manager
            case eitherDecode (responseBody res) of
                Left err -> return $ Left err
                Right items ->
                    let newAcc = acc ++ items
                    in if null items || length newAcc >= limit
                        then return $ Right (take limit newAcc)
                        else go manager (page + 1) newAcc


getUser :: String -> Maybe String -> IO (Either String User)
getUser username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username) mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getRepos :: Int -> String -> Maybe String -> IO (Either String [Repo])
getRepos limit username mToken =
    paginate limit ("https://api.github.com/users/" ++ username ++ "/repos") mToken

getRepoLanguages :: String -> Maybe String -> IO (Either String (Map.Map String Int))
getRepoLanguages url mToken = do
    manager <- newTlsManager
    req <- buildRequest url mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getStarredRepos :: Int -> String -> Maybe String -> IO (Either String [Repo])
getStarredRepos limit username mToken =
    paginate limit ("https://api.github.com/users/" ++ username ++ "/starred") mToken

getFollowers :: Int -> String -> Maybe String -> IO (Either String [User])
getFollowers limit username mToken =
    paginate limit ("https://api.github.com/users/" ++ username ++ "/followers") mToken

getFollowing :: Int -> String -> Maybe String -> IO (Either String [User])
getFollowing limit username mToken =
    paginate limit ("https://api.github.com/users/" ++ username ++ "/following") mToken

getOrgs :: Int -> String -> Maybe String -> IO (Either String [Org])
getOrgs limit username mToken =
    paginate limit ("https://api.github.com/users/" ++ username ++ "/orgs") mToken
