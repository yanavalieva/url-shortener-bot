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

import DataBase.Requests(Config,initializeDB)

data BotConfig = BotConfig {
        stManager :: Manager
      , stToken :: Token
      , stOffset  :: Maybe Int
      , stLongPoolTimeout :: Maybe Int
      , dbConfig :: Config
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
  c <- initializeDB
  return BotConfig { stManager = manager
                   , stToken = token
                   , stOffset = Nothing
                   , stLongPoolTimeout = Just 25
                   , dbConfig = c
                   }
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

unliftBotIO :: BotT IO a -> Bot (IO a)
unliftBotIO b = fmap (evalStateT $ runBT b) get

asyncBot :: BotT IO a -> BotT IO (Async a)
asyncBot = liftBotIO . fmap async . unliftBotIO

-- send message to specified chat
sendBotMessage :: Show a => a -> Text -> BotT IO MessageResponse
sendBotMessage chatId txt = do
  s <- get
  let textChatId = fromString $ show chatId
  let msgReq = (sendMessageRequest textChatId txt) { message_disable_web_page_preview = Just True }
  let r = sendMessage (stToken s) msgReq (stManager s)
  liftIO $ eitherThrowLog r

-- get updates using long pooling
getBotUpd :: BotT IO UpdatesResponse
getBotUpd = do
  s <- get
  let r = getUpdates (stToken s) (stOffset s) Nothing (stLongPoolTimeout s) (stManager s)
  liftIO $ eitherThrowLog r

runBotInteractive :: (Int -> Config -> Text -> IO Text) -> IO ()
runBotInteractive mHandler = do
    cfg <- liftIO createBotConfig
    evalStateT (runBT $ forever bot) cfg
  where
    bot :: BotT IO ()
    bot = do
            let unwrapMsgUpdate = fmap getMsgText . result
            liftIO $ debugM loggerName "Send longpool request.."
            upds <- unwrapMsgUpdate <$> getBotUpd
            liftIO $ debugM loggerName $ "Updates recivied: "++ show (length upds)
            mapM_ (updateOffset . fst) upds
            let inMsg = mapMaybe snd upds
            dbc <- dbConfig <$> get
            let processMsg (uid, cid, txt) = asyncBot $ liftIO (mHandler uid dbc txt) >>= sendBotMessage cid
            -- let processMsg = uncurry $ (asyncBot.) . (liftIO . (mHandler dbc) >=>) . sendBotMessage
            mapM_ processMsg inMsg
    --                       update id, maybe (user_id, char_id, message_text)
    getMsgText :: Update -> (Int, Maybe (Int, Int, Text))
    getMsgText u = (update_id u, message u >>= (\m -> (chat_id $ chat m, ,) <$> (user_id <$> from m) <*> text m))
