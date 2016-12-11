{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Bot
    ( runBotInteractive
    ) where

import Network.HTTP.Client(newManager,Manager)
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Telegram.API.Bot
import Servant.Common.Req(ServantError)
import System.Environment(getEnv)
import System.IO.Error(isDoesNotExistError)
import Data.String(fromString, IsString)
import Data.Maybe
import Data.Text(Text)
import Data.Text.Read
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import qualified Control.Exception as E
import System.Log.Logger
import Control.Concurrent
import Control.Concurrent.Async

data BotConfig = BotConfig {
        stManager :: Manager
      , stToken :: Token
      , stOffset  :: Maybe Int
      , stLongPoolTimeout :: Maybe Int
    }

newtype BotT m a = BotT {
          runBT :: StateT BotConfig m a
        } deriving (Functor, Applicative, Monad,
                    MonadTrans,
                    MonadState BotConfig)


instance (MonadIO m ) => MonadIO (BotT m) where
  liftIO = lift . liftIO

type Bot a = BotT Identity a

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
                   , stLongPoolTimeout = Just 25 }
  where
    getTokenFromEnv = fmap (Token . fromString) $ getEnv "TELEGRAM_TOKEN" `E.catch` returnDefaultToken
    returnDefaultToken :: E.IOException -> IO String
    returnDefaultToken e
      | isDoesNotExistError e = return defaultToken
      | otherwise = E.throw e
    defaultToken = "bot326648651:AAFwwJ1hMj0T1zBYGyOz0uJOFvyXgibSKdc"

-- Updates last processed update id
updateOffset :: (Monad m) => Int -> BotT m ()
updateOffset n = modify $ modifyOffset $ Just . maxMaybe (n+1)
  where
    modifyOffset f s = s { stOffset = f (stOffset s) }
    maxMaybe a (Just b) = max a b
    maxMaybe a Nothing = a

eitherThrowLog :: (E.Exception e) => IO (Either e a) -> IO a
eitherThrowLog m =  m >>= (\e -> logOnErr e >> return (either E.throw id e))
  where
    logOnErr :: (Show e) => Either e a -> IO ()
    logOnErr (Left e) = errorM loggerName (show e)
    logOnErr (Right _) = return ()

liftBotIO :: Bot (IO a) -> BotT IO a
liftBotIO b = get >>= liftIO . evalState (runBT b)

asyncBot :: Bot (IO a) -> BotT IO (Async a)
asyncBot = liftBotIO . fmap async

-- send message to specified chat
sendBotMessage :: Show a => a -> Text -> Bot (IO MessageResponse)
sendBotMessage chatId txt = do
  s <- get
  let textChatId = fromString $ show chatId
  let msgReq = sendMessageRequest textChatId txt
  let r = sendMessage (stToken s) msgReq (stManager s)
  return $ eitherThrowLog r

-- get updates using long pooling
getBotUpd :: Bot (IO UpdatesResponse)
getBotUpd = do
  s <- get
  let r = getUpdates (stToken s) (stOffset s) Nothing (stLongPoolTimeout s) (stManager s)
  return $ eitherThrowLog r

runBotInteractive :: (Text -> IO Text) -> IO ()
runBotInteractive textMap = do
    cfg <- liftIO createBotConfig
    evalStateT (runBT $ forever bot) cfg
  where
    bot :: BotT IO ()
    bot = do
            let unwrapMsgUpdate = fmap getMsgText . result
            liftIO $ debugM loggerName "Send longpool request.."
            upds <- unwrapMsgUpdate <$> liftBotIO getBotUpd
            liftIO $ debugM loggerName $ "Updates recivied: "++ show (length upds)
            mapM_ (updateOffset . fst) upds
            let inMsg = mapMaybe snd upds
            mapM_ (\(b,c) -> do
                                t <- liftIO (textMap c)
                                asyncBot $ sendBotMessage b t) inMsg
    getMsgText :: Update -> (Int, Maybe (Int, Text))
    getMsgText u = (update_id u, message u >>= (\m -> (chat_id $ chat m, ) <$> text m))
