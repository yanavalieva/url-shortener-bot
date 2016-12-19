module UrlShort.Qps (qps) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)

qps :: [Char] -> IO (Either Text Text)
qps [] = return $ Left $ pack "Invalid Link"
qps url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ url
    response <- httpLbs request manager

    let status = statusCode $ responseStatus response

    return $ if status == 200
                then Right $ decodeUtf8 $ responseBody response
                else Left $ pack "Unknown error"