{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client(newManager,Manager)

import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Telegram.API.Bot
import Servant.Common.Req(ServantError)

import System.Environment(getEnv)

import qualified Control.Exception as E
import System.IO.Error(isDoesNotExistError)

import Data.String(fromString)

import Data.Either
import Data.Text

import Control.Monad(liftM)
import Control.Monad.State
-- import Control.Monad.Writer
-- import Control.Monad.Reader

import Lib
import TinyUrl

data BotConfig = BotConfig {
        stManager :: Manager
      , stToken :: Token
    }

createBotConfig :: IO BotConfig
createBotConfig = do
  token <- liftM (Token . fromString) $ getEnv "TELEGRAM_TOKEN" `E.catch` returnDefaultToken
  manager <- newManager tlsManagerSettings
  return $ BotConfig { stManager = manager
                     , stToken = token }
  where
    returnDefaultToken :: E.IOException -> IO String
    returnDefaultToken e
      | isDoesNotExistError e = return "326648651:AAFwwJ1hMj0T1zBYGyOz0uJOFvyXgibSKdc"
      | otherwise = E.throw e

type Bot a = StateT BotConfig IO a

getBotUpd :: Bot (Either ServantError UpdatesResponse)
getBotUpd = do
  s <- get
  res <- liftIO $ getUpdates (stToken s) Nothing Nothing Nothing (stManager s)
  return res

getTextMessage :: Maybe Message -> Maybe Text
getTextMessage (Just (Message { text = t })) = t
getTextMessage _ = Nothing

processUpd :: Update -> IO ()
processUpd u = print $ getTextMessage $ message u

main :: IO ()
main = do
  botCfg <- createBotConfig
  r <- evalStateT getBotUpd botCfg
  case r of
    Right (Response upds) -> mapM_ processUpd upds
    Left e -> print e


