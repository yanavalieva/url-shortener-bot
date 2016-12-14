{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Bitly (getShortUrl) where

import Network.HTTP.Client
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Internal
import Data.Aeson
import Data.Text.Internal.Lazy (Text)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Text.Lazy.Encoding (encodeUtf8)

data Data =
    Data { long_url :: !Text
         , url :: !Text
         , hash :: !Text
         , global_hash :: !Text
         , new_hash :: Int
        } deriving (Show, Generic)

instance FromJSON Data
instance ToJSON Data

data ResponseBody =
    ResponseBody { status_code :: Int
                 , status_txt :: !Text
                 , _data :: Data
                } deriving (Show, Generic)

instance FromJSON ResponseBody where
    parseJSON (Object v) =
        ResponseBody <$> v .: "status_code"
                     <*> v .: "status_txt"
                     <*> v .: "data"
    parseJSON _ = mzero

instance ToJSON ResponseBody where
 toJSON (ResponseBody status_code status_txt _data) =
    object [ "status_code"  .= status_code
           , "status_txt"   .= status_txt
           , "data"         .= _data
             ]

authToken :: String
authToken = "debe319f92b9d2d1109a9958a18985fede11b4ff"

getShortUrl :: [Char] -> IO (Maybe ByteString)
getShortUrl longUrl = do
    manager <- newManager tlsManagerSettings

    let encodedUrl = urlEncode longUrl
    request <- parseRequest 
                $ "https://api-ssl.bitly.com/v3/shorten?access_token=" 
                    ++ authToken 
                    ++ "&longUrl=" 
                    ++ encodedUrl

    response <- httpLbs request manager

    let status = statusCode $ responseStatus response
    
    if status == 200
        then do
            let eitherBody = (eitherDecode $ responseBody response) :: Either String ResponseBody
            case eitherBody of
                Left _ -> return Nothing
                Right body -> return $ Just $ encodeUtf8 $ url $ _data body
        else return Nothing