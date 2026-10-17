{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHub.Client where

import           Data.Aeson                (eitherDecode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.Map                  as Map

import           Network.HTTP.Client
    (Request, httpLbs, parseRequest, requestHeaders, responseBody)
import           Network.HTTP.Client.TLS   (newTlsManager)
import           Network.HTTP.Types.Header (hAuthorization, hUserAgent)

import           GitHub.Types              (Org, Repo, User)

buildRequest :: String -> Maybe String -> IO Request
buildRequest url mToken = do
    initialRequest <- parseRequest url
    let baseHeaders = [(hUserAgent, "gh-api-cli")]
    let authHeaders = case mToken of
                Nothing    -> []
                Just token -> [(hAuthorization, BC.pack $ "Bearer " ++ token)]
    return initialRequest { requestHeaders = baseHeaders ++ authHeaders }

getUser :: String -> Maybe String -> IO (Either String User)
getUser username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username) mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getRepos :: String -> Maybe String -> IO (Either String [Repo])
getRepos username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username ++ "/repos?per_page=100") mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getRepoLanguages :: String -> Maybe String -> IO (Either String (Map.Map String Int))
getRepoLanguages url mToken = do
    manager <- newTlsManager
    req <- buildRequest url mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getStarredRepos :: String -> Maybe String -> IO (Either String [Repo])
getStarredRepos username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username ++ "/starred?per_page=100") mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getFollowers :: String -> Maybe String -> IO (Either String [User])
getFollowers username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username ++ "/followers?per_page=100") mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getFollowing :: String -> Maybe String -> IO (Either String [User])
getFollowing username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username ++ "/following?per_page=100") mToken
    (eitherDecode . responseBody) <$> httpLbs req manager

getOrgs :: String -> Maybe String -> IO (Either String [Org])
getOrgs username mToken = do
    manager <- newTlsManager
    req <- buildRequest ("https://api.github.com/users/" ++ username ++ "/orgs?per_page=100") mToken
    (eitherDecode . responseBody) <$> httpLbs req manager
