module UrlShort.Qps (qps) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy
import Data.ByteString
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)

qps :: Text -> IO (Either Text Text)
qps url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $ "http://qps.ru/api?url=" ++ (Data.Text.unpack url)
    response <- httpLbs request manager

    let status = statusCode $ responseStatus response

    return $ if status == 200
                then Right $ decodeUtf8 $ toStrict $ responseBody response
                else Left $ Data.Text.pack "Unknown error"