module Qps (getShortUrl) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal

getShortUrl :: [Char] -> IO ByteString
getShortUrl url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ url
    response <- httpLbs request manager

    return $ responseBody response