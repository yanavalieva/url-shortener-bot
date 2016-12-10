{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Bot
    ( runBot
    ) where

import Network.HTTP.Client(newManager,Manager)
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Telegram.API.Bot
import Servant.Common.Req(ServantError)
import System.Environment(getEnv)
import System.IO.Error(isDoesNotExistError)
import Data.String(fromString, IsString)
import Data.Either
import Data.Maybe
import Data.Char(isDigit)
import Data.Text(Text)
import Data.Text as T(all)
import Data.Text.Read
import Control.Monad(liftM)
import Control.Monad.State
import Control.Applicative
import Control.Monad.Trans.Maybe
import qualified Control.Exception as E
import System.Log.Logger

data BotConfig = BotConfig {
        stManager :: Manager
      , stToken :: Token
      , stOffset  :: Maybe Int
      , stLongPoolTimeout :: Maybe Int
    }

newtype Bot a = Bot {
          runB :: StateT BotConfig IO a
        } deriving (Functor, Applicative, Monad,
                    MonadIO,
                    MonadState BotConfig)

loggerName :: String
loggerName = "App.Bot"

createBotConfig :: IO BotConfig
createBotConfig = do
  updateGlobalLogger loggerName (setLevel DEBUG)
  token <- getTokenFromEnv
  debugM loggerName $ "Using token" ++ show token
  manager <- newManager tlsManagerSettings
  return BotConfig { stManager = manager
                   , stToken = token
                   , stOffset = Nothing
                   , stLongPoolTimeout = Just 10 }
  where
    getTokenFromEnv = fmap (Token . fromString) $ getEnv "TELEGRAM_TOKEN" `E.catch` returnDefaultToken
    returnDefaultToken :: E.IOException -> IO String
    returnDefaultToken e
      | isDoesNotExistError e = return defaultToken
      | otherwise = E.throw e
    defaultToken = "bot326648651:AAFwwJ1hMj0T1zBYGyOz0uJOFvyXgibSKdc"

sendBotMessage :: Show a => a -> Text -> Bot (Either ServantError MessageResponse)
sendBotMessage chatId txt = do
  s <- get
  let textChatId = fromString $ show chatId
  let msgReq = sendMessageRequest textChatId txt
  liftIO $ sendMessage (stToken s) msgReq (stManager s)

updateOffset :: Int -> Bot ()
updateOffset n = modify $ modifyOffset $ Just . maxMaybe (n+1)
  where
    modifyOffset f s = s { stOffset = f (stOffset s) }
    maxMaybe a (Just b) = max a b
    maxMaybe a Nothing = a

processUpdMsg :: Update -> MaybeT Bot (Int,Text)
processUpdMsg u@Update { update_id=uid, message=msg } = do
  liftIO $ debugM loggerName $ "Process update #" ++ show uid
  lift $ updateOffset uid
  let inMsg = msg >>= (\m -> (chat_id $ chat m, ) <$> text m)
  guard (isJust inMsg)
  return $ fromJust inMsg

dummy n = show n ++ " " ++ (show $ length $ show $ product [1..n])

getBotUpd :: Bot (Either ServantError UpdatesResponse)
getBotUpd = do
  liftIO $ debugM loggerName "Send longpool request..."
  getBotUpd_
  where
    getBotUpd_ = get >>=
      liftIO . (\s ->
        getUpdates (stToken s) (stOffset s) Nothing (stLongPoolTimeout s) (stManager s))

type TextMessageHandler r = Int -> Text -> Bot r

processBotUpds :: TextMessageHandler r -> Bot [r]
processBotUpds h = do
  r <- getBotUpd
  case r of
    Right (Response upds) -> do
                  liftIO $ debugM loggerName $ "Get response. Updates: " ++ (show . length) upds
                  catMaybes <$> mapM (runMaybeT . (processUpdMsg >=> (lift . uncurry h))) upds
    Left e -> do
      liftIO $ errorM loggerName $ show e
      return []

onTextMessage :: TextMessageHandler ()
onTextMessage cid txt = do
  r <- sendBotMessage cid txt
  return ()

runBot = createBotConfig >>= evalStateT (runB $ forever $ processBotUpds onTextMessage)
