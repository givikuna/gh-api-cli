{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitHub.Client where

import           Data.Aeson                (eitherDecode)

import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BSL

import           Network.HTTP.Client
    (Manager, Request, Response, httpLbs, parseRequest, requestHeaders, responseBody)
import           Network.HTTP.Client.TLS   (newTlsManager)
import           Network.HTTP.Types.Header (HeaderName, hAuthorization, hUserAgent)

import           GitHub.Types              (Repo, User)

-- httpLbs takes in a request & a manager
-- & it makes a connection & returns an IO response

getUser :: String -> Maybe String -> IO (Either String User)
getUser username mToken = do
    manager :: Manager <- newTlsManager

    initialRequest :: Request <- parseRequest $ "https://api.github.com/users/" ++ username

    let baseHeaders :: [(HeaderName, BC.ByteString)] = [(hUserAgent, "gh-api-cli")]

    let authHeaders ::  [(HeaderName, BC.ByteString)] = case mToken of
            Nothing    -> []
            Just token -> [(hAuthorization, BC.pack $ "Bearer " ++ token)]

    let request :: Request = initialRequest { requestHeaders = baseHeaders ++ authHeaders }

    response :: Response BSL.ByteString <- httpLbs request manager

    return $ eitherDecode (responseBody response)

getRepos :: String -> Maybe String -> IO (Either String [Repo])
getRepos username mToken = do
    manager :: Manager <- newTlsManager

    initialRequest :: Request <- parseRequest
        $ "https://api.github.com/users/" ++ username ++ "/repos?per_page=100"

    let baseHeaders :: [(HeaderName, BC.ByteString)] = [(hUserAgent, "gh-api-cli")]
    let authHeaders :: [(HeaderName, BC.ByteString)] = case mToken of
                Nothing    -> []
                Just token -> [(hAuthorization, BC.pack $ "Bearer " ++ token)]

    let request :: Request = initialRequest { requestHeaders = baseHeaders ++ authHeaders }

    {-
    response <- httpLbs request manager
    BC.putStrLn $ BSL.toStrict (responseBody response)
    -}

    (eitherDecode . responseBody) <$> httpLbs request manager
