module Bitly (getShortUrl) where

import Network.HTTP.Client
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal
import Data.Aeson

authToken :: String
authToken = "debe319f92b9d2d1109a9958a18985fede11b4ff"

getShortUrl :: [Char] -> IO ByteString
getShortUrl url = do
    manager <- newManager tlsManagerSettings

    let encodedUrl = urlEncode url
    request <- parseRequest 
                $ "https://api-ssl.bitly.com/v3/shorten?access_token=" 
                    ++ authToken 
                    ++ "&longUrl=" 
                    ++ encodedUrl

    response <- httpLbs request manager

    return $ responseBody response