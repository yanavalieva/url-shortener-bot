{-# LANGUAGE OverloadedStrings #-}

module Google (getShortUrl) where

import           Data.Aeson                 (encode, object, (.=))
import           Data.ByteString.Lazy.Internal
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

authKey :: String
authKey = "AIzaSyD0ZGwJBT3dO_VWR9QqbJjS9CJJ4GX1zOc"

getShortUrl :: [Char] -> IO ByteString
getShortUrl longUrl = do
    manager <- newManager tlsManagerSettings

    let requestObject = object [ "longUrl" .= (longUrl :: String)]

    initialRequest <- parseRequest $ "https://www.googleapis.com/urlshortener/v1/url?key=" ++ authKey

    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders = 
                [ ("Content-Type", "application/json; charset=utf-8")
                ]
            }

    response <- httpLbs request manager
    return $ responseBody response