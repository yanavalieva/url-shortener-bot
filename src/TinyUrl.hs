{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}

module TinyUrl
  (tinyUrl
  ) where

import System.Environment(getArgs)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Data.ByteString.Lazy.Char8(unpack,pack,ByteString)
import Data.String(fromString)

import qualified Control.Exception as E
import Control.Monad(liftM)

tinyUrl :: String -> IO (Maybe String)
tinyUrl url = do
  m <- newManager $ defaultManagerSettings { managerResponseTimeout = Just $ 1e6 }
  let rurl = fromString ("http://tinyurl.com/api-create.php?url=" ++ url)
  response <- E.try $ httpLbs rurl m ::
            (IO (Either E.SomeException (Response ByteString))) -- TODO look for specified exteption
  return $ eitherToMaybe $ fmap (unpack . responseBody) response

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right r) = Just r
eitherToMaybe _ = Nothing