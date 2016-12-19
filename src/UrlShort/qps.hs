module UrlShort.Qps (qps) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

qps :: Text -> IO (Either Text Text)
qps url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ (unpack url)
    response <- httpLbs request manager

    let status = statusCode $ responseStatus response

    return $ if status == 200
                then Right $ decodeUtf8 $ responseBody response
                else Left $ pack "Unknown error"