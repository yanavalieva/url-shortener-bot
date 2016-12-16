module Qps (qps) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal
import Data.ByteString.Lazy.Char8 (unpack)

qps :: [Char] -> IO (Either String String)
qps url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ url
    response <- httpLbs request manager

    let status = statusCode $ responseStatus response

    if status == 200
        then return $ Right $ unpack $ responseBody response
        else return $ Left "Unknown error"

    return $ if status == 200
                then Right $ unpack $ responseBody response
                else Left "Unknown error"