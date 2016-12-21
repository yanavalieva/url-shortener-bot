{-# LANGUAGE OverloadedStrings #-}
--  LANGUAGE NumDecimals #

module Qr(goqr) where


--import System.Environment(getArgs)
--import Network.HTTP.Client
--import Network.HTTP.Types.Status (statusCode)

--import Data.ByteString.Char8(unpack,pack,ByteString)
--import Data.String(fromString)

--import qualified Control.Exception as E
--import Control.Monad(liftM)

import Data.Text(Text)
import qualified Data.Text as T
import Data.ByteString(ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

goqr :: Text -> IO ByteString
goqr d = do
    manager <- newManager tlsManagerSettings
    let rurl = concat ["http://api.qrserver.com/v1/create-qr-code/?data=", d, "&size=300x300"]
    undefined


--
-- tinyUrl :: String -> IO (Maybe String)
-- tinyUrl url = do
--   m <- newManager $ defaultManagerSettings { managerResponseTimeout = Just $ 1e6 }
--
--   response <- E.try $ httpLbs rurl m ::
--             (IO (Either E.SomeException (Response ByteString))) -- TODO look for specified exteption
--   return $ eitherToMaybe $ fmap (unpack . responseBody) response
--
-- eitherToMaybe :: Either e a -> Maybe a
-- eitherToMaybe (Right r) = Just r
-- eitherToMaybe _ = Nothing
