{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client(newManager,Manager)

import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Telegram.API.Bot
import Servant.Common.Req(ServantError)

import System.Environment(getEnv)

import qualified Control.Exception as E
import System.IO.Error(isDoesNotExistError)

import Data.String(fromString, IsString)

import Data.Either
import Data.Maybe
import Data.Text(Text)

import Control.Monad(liftM)
import Control.Monad.State
-- import Control.Monad.Writer
-- import Control.Monad.Reader

import Lib
import TinyUrl

data BotConfig = BotConfig {
        stManager :: Manager
      , stToken :: Token
      , stOffset  :: Maybe Int
    }

type Bot a = StateT BotConfig IO a

createBotConfig :: IO BotConfig
createBotConfig = do
  token <- liftM (Token . fromString) $ getEnv "TELEGRAM_TOKEN" `E.catch` returnDefaultToken
  print token
  manager <- newManager tlsManagerSettings
  return BotConfig { stManager = manager
                   , stToken = token
                   , stOffset = Nothing }
  where
    returnDefaultToken :: E.IOException -> IO String
    returnDefaultToken e
      | isDoesNotExistError e = return defaultToken
      | otherwise = E.throw e
    defaultToken = "bot326648651:AAFwwJ1hMj0T1zBYGyOz0uJOFvyXgibSKdc"

getBotUpd :: Bot (Either ServantError UpdatesResponse)
getBotUpd = do
  s <- get
  liftIO $ getUpdates (stToken s) (stOffset s) Nothing (Just 10) (stManager s)

sendBotMessage :: String -> Text -> Bot (Either ServantError MessageResponse)
sendBotMessage chatId txt = do
  s <- get
  let msgReq = sendMessageRequest (fromString chatId) txt
  liftIO $ sendMessage (stToken s) msgReq (stManager s)

processUpd :: Update -> Bot ()
processUpd u = do
  liftIO $ print u
  let r = processMsg $ message u
  liftIO $ print r
  let uid = update_id u
  modify (\s -> s { stOffset = Just $ uid+1})
  liftIO $ print uid
  when (isJust r) $ do
      let Just (c,t) = r
      r <- sendBotMessage (show c) t
      return ()
  return ()
  where
    processMsg (Just Message{text=Just m, chat=Chat{chat_id = cid}}) = Just (cid, m)
    processMsg _ = Nothing

processBotUpd :: Bot ()
processBotUpd = do
  r <- getBotUpd
  case r of
    Right (Response upds) -> do
                  liftIO $ print $ length upds
                  mapM_ processUpd upds
    Left e -> liftIO $ print e
  return ()

main :: IO ()
main = do
  botCfg <- createBotConfig
  evalStateT (forever processBotUpd) botCfg
  print "done"
