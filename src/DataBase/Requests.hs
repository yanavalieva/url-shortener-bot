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
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
import GHC.Generics (Generic) 
import DataBase.Scheme

data Config = Config { connections :: ConnectionPool }

newtype App a = App { 
    runApp :: ReaderT Config IO a 
    } deriving (Functor, Applicative, Monad, 
                MonadIO, MonadReader Config )

createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $ createSqlitePool "test.db3" 10

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks connections
    liftIO $ runSqlPool query pool

runRequest :: MonadIO m => SqlPersistT IO a -> Config -> m a
runRequest req cfg = runReaderT (runDb req) cfg

createUser :: MonadIO m => Int64 -> String -> ReaderT SqlBackend m Int64
createUser id def_serv = do
    newUser <- insert $ User id def_serv
    return $ fromSqlKey newUser

insertIntoHistory :: MonadIO m => (Key User) -> String -> String -> String -> ReaderT SqlBackend m Int64
insertIntoHistory id serv src short = do
    newHist <- insert $ History id serv src short
    return $ fromSqlKey newHist

createOrFindUser :: MonadIO m => Int64 -> Config -> m String
createOrFindUser id cfg = do
    usr <- runRequest (getBy $ UniqueUser 1) cfg
    case usr of
        Nothing -> do
            runRequest (createUser id "google") cfg
            return "google"
        Just (Entity _ (User _ service)) -> return service

findInHistory :: MonadIO m => Key User -> String -> String -> Config -> m [String]
findInHistory id serv src cfg = 
    runRequest (selectList [
        HistoryUser_id ==. id, 
        HistoryService ==. serv,
        HistorySrc_url ==. src
        ] []) cfg >>= return . map (\(Entity _ (History _ _ _ short)) -> short)

setNewDefault :: MonadIO m => Int64 -> String -> Config -> m ()
setNewDefault id serv cfg =  
    runRequest (updateWhere 
        [UserTelegram_id ==. id] 
        [UserDefault_service =. serv]
    ) cfg

main = do
    pool <- createPool
    let cfg = Config { connections = pool }
    runSqlPool runMigrations pool
    setNewDefault 1 "bitly" cfg