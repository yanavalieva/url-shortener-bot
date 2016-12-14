module Qps (getShortUrl) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal

getShortUrl :: [Char] -> IO (Maybe ByteString)
getShortUrl url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ url
    response <- httpLbs request manager

    let status = statusCode $ responseStatus response
    
    if status == 200
        then return $ Just $ responseBody response
        else return Nothing