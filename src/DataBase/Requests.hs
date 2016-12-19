{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-} 

module DataBase.Requests where 

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Reader.Class
import System.Environment
import Control.Monad.IO.Class
import Data.Int 
import Data.Text hiding (map)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
import GHC.Generics (Generic) 
import DataBase.Scheme
import Service

data Config = Config { connections :: ConnectionPool }

newtype App a = App { 
    runApp :: ReaderT Config IO a 
    } deriving (Functor, Applicative, Monad, 
                MonadIO, MonadReader Config )

createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $ createSqlitePool "users.db3" 10

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks connections
    liftIO $ runSqlPool query pool

runRequest :: MonadIO m => SqlPersistT IO a -> Config -> m a
runRequest req cfg = runReaderT (runDb req) cfg

-- запрос создания пользователя
createUser :: MonadIO m => Int64 -> Service -> ReaderT SqlBackend m Int64
createUser id def_serv = do
    newUser <- insert $ User id def_serv
    return $ fromSqlKey newUser

-- запрос добавления записи в историю
insertIntoHistory :: MonadIO m => Int64 -> Service -> Text -> Text -> ReaderT SqlBackend m Int64
insertIntoHistory id serv src short = do
    newHist <- insert $ History id serv src short
    return $ fromSqlKey newHist

-- создание нового пользователя или поиск сервиса по умолчанию уже существующего пользователя
createOrFindUser :: MonadIO m => Int64 -> Config -> m Service
createOrFindUser id cfg = do
    usr <- runRequest (getBy $ UniqueUser id) cfg
    case usr of
        Nothing -> do
            runRequest (createUser id Google) cfg
            return Google
        Just (Entity _ (User _ service)) -> return service

-- поиск по истории
findInHistory :: MonadIO m => Int64 -> Service -> Text -> Config -> m [Text]
findInHistory id serv src cfg = 
    runRequest (selectList [
        HistoryUserId ==. id, 
        HistoryService ==. serv,
        HistorySrcUrl ==. src
        ] []) cfg >>= return . map (\(Entity _ (History _ _ _ short)) -> short)

-- установка нового сервиса по умолчанию
setNewDefault :: MonadIO m => Int64 -> Service -> Config -> m ()
setNewDefault id serv cfg =  
    runRequest (updateWhere 
        [UserTelegramId ==. id] 
        [UserDefaultService =. serv]
    ) cfg

-- добавление записи в историю
createHistoryRecord :: MonadIO m =>
     Int64 -> Service -> Text -> Text -> Config -> m Int64
createHistoryRecord id serv src short = 
    runRequest (insertIntoHistory id serv src short)

{- main = do
    pool <- createPool
    let cfg = Config { connections = pool }
    runSqlPool runMigrations pool
    findInHistory 1 Bitly "src.com" cfg -}